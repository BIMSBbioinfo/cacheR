#' Create a disk-backed caching decorator for a function
#'
#' `cacheFile()` creates a decorator that caches function results to files
#' in `cache_dir`, keyed by the function body and arguments. It also records
#' parent-child relationships between cached calls so that a "cache tree"
#' can be reconstructed.
#'
#' Typical usage (with the `%@%` decorator operator) is:
#'
#' ```r
#' inner_fun <- cacheFile(cache_dir) %@% function(x) x + 1
#' outer_fun <- cacheFile(cache_dir) %@% function(x) inner_fun(x) * 2
#' ```
#'
#'
#' @param cache_dir Directory where cache files will be stored.
#' @param ... Additional options controlling cache behavior (e.g. hashing,
#'   compression, invalidation rules).
#'
#' @return A decorator object that can be combined with a function using
#'   `%@%`, returning a cached version of that function.
#' @export
cacheFile <- function(
    cache_dir = NULL,
    backend = getOption("cacheR.backend", "rds")
  ) decorator %@% function(f) {

  if (is.null(cache_dir)) {
      cache_dir <- cacheR_default_dir()
  }else{
    if(!file.exists(cache_dir))
      stop("the designated cache_dir does not exist")
  }
  # possibly normalize the path
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)



  backend <- match.arg(backend, c("rds", "qs"))

  # capture formal arguments and body as in your original
  argnames <- head(as.list(args(as.list(environment())[[1]])), -1)
  fbody    <- lapply(as.list(body(f)), as.character)

  function(..., .load = TRUE, .anames = argnames, .fbody = fbody) {

    # ---- reconstruct the call & arguments (your original logic) ---------
    fcall <- as.list(match.call())

    fname <- fcall[[1]]
    args  <- fcall[-1]

    if (!is.null(names(args)) && any(names(args) == ".load"))
      args <- args[names(args) != ".load"]

    if (!is.null(names(args))) {
      named_args <- setdiff(names(args), "")
      if (!is.null(named_args)) {
        for (i in named_args)
          .anames[[i]] <- args[[i]]
      }

      pos_args <- which(names(args) == "")
      if (length(pos_args) > 0) {
        for (i in pos_args)
          .anames[[i]] <- args[[i]]
      }
    } else {
      for (i in seq_along(args))
        .anames[[i]] <- args[[i]]
    }

    .dotind <- names(.anames) == "..."
    if (any(.dotind)) {
      .anames <- .anames[!.dotind]
    }

    if (length(args) > 0) {
      for (i in seq_along(.anames)) {
        if (is.call(.anames[[i]]) || is.name(.anames[[i]])) {
          val <- eval(.anames[[i]], envir = parent.frame())
          if (is.null(val)) val <- list(NULL)
          .anames[[i]] <- val
        }
      }
    }

    # ---- compute hash & output path -------------------------------------
    hashlist  <- list(anames = .anames, body = .fbody)
    args_hash <- digest::digest(hashlist, algo = "md5")
    message(args_hash)

    outfile <- file.path(
      inpath,
      paste(as.character(fname), args_hash, "rds", sep = ".")
    )

    # ---- register node + manage call stack ------------------------------
    node_id <- paste(as.character(fname), args_hash, sep = ":")

    .cacheTree_register_node(
      node_id   = node_id,
      fname     = fname,
      args_hash = args_hash,
      outfile   = outfile
    )

    .cacheTree_env$call_stack <- c(.cacheTree_env$call_stack, node_id)
    on.exit({
      .cacheTree_env$call_stack <- head(.cacheTree_env$call_stack, -1L)
    }, add = TRUE)

    # ---- caching logic (unchanged semantics) ----------------------------
    if (.load && file.exists(outfile)) {
      message(paste0(fname, ": Returning loaded data ..."))
      message(outfile)
      .cacheR_read(outfile)$dat
    } else {
      message(paste0(fname, ": Running function ..."))
      dat <- f(...)

      .cacheR_write(list(dat = dat, args = .anames, body = .fbody), outfile)
      dat
    }
  }
}
