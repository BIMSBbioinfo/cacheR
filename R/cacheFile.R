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
    backend   = getOption("cacheR.backend", "rds"),
    file_args = NULL  # names of args that are directories
  ) decorator %@% function(f) {

  if (is.null(cache_dir)) {
    cache_dir <- cacheR_default_dir()
  } else {
    if (!file.exists(cache_dir))
      stop("the designated cache_dir does not exist")
  }
  # normalize the path
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  backend <- match.arg(backend, c("rds", "qs"))

  # --- helper: find literal directory strings in the function body ------
  .find_literal_dirs <- function(expr) {
    res <- character()

    walk <- function(e) {
      if (is.call(e)) {
        fname <- as.character(e[[1]])[1]

        # look at list.files(), dir(), list.dirs()
        if (fname %in% c("list.files", "dir", "list.dirs")) {
          args <- as.list(e)[-1]
          arg_names <- names(args)
          target <- NULL

          # named 'path' argument if present, otherwise first positional
          if (!is.null(arg_names) && "path" %in% arg_names) {
            target <- args[["path"]]
          } else if (length(args) >= 1) {
            target <- args[[1]]
          }

          if (!is.null(target) && is.character(target) && length(target) == 1L) {
            res <<- c(res, target)
          }
        }

        # recurse into all subexpressions
        for (el in as.list(e)) walk(el)
      }
    }

    walk(expr)
    unique(res)
  }

  # --- helper: find package dependencies of a function -------------------
  .find_package_deps <- function(fun) {
    if (!requireNamespace("codetools", quietly = TRUE)) {
      warning("codetools not available; package dependencies will not be tracked.")
      return(NULL)
    }

    globs <- codetools::findGlobals(fun, merge = FALSE)
    fun_names <- unique(globs$functions)

    if (!length(fun_names)) return(NULL)

    pkgs <- unique(unlist(lapply(fun_names, function(sym) {
      ga <- getAnywhere(sym)

      if (!length(ga$where)) return(NULL)

      ns <- ga$where[grepl("^package:", ga$where)]
      if (!length(ns)) return(NULL)

      sub("^package:", "", ns)
    })))

    if (!length(pkgs)) return(NULL)

    # ignore base-ish packages
    pkgs <- setdiff(pkgs, c("base", "stats", "utils",
                            "graphics", "grDevices", "methods"))
    if (!length(pkgs)) return(NULL)

    versions <- vapply(pkgs, function(p) {
      as.character(utils::packageVersion(p))
    }, character(1L))

    # deterministic ordering
    ord <- order(pkgs)
    pkgs <- pkgs[ord]
    versions <- versions[ord]

    data.frame(
      package = pkgs,
      version = versions,
      stringsAsFactors = FALSE
    )
  }

  # capture formal arguments and body as in your original
  argnames <- head(as.list(args(as.list(environment())[[1]])), -1)
  fbody    <- lapply(as.list(body(f)), as.character)

  # literal directories hardcoded in the body, e.g. list.files("/some/path")
  # (assuming .find_path_specs is defined elsewhere in your file, as before)
  path_specs      <- .find_path_specs(body(f))
  static_dirs_lit <- path_specs$literals       # "/hard/coded/path"
  static_dirs_sym <- path_specs$symbols

  # NEW: compute package dependencies once when decorator is created
  pkg_deps <- .find_package_deps(f)

  function(...,
           .load     = TRUE,
           .anames   = argnames,
           .fbody    = fbody,
           .pkg_deps = pkg_deps) {

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

    # ---- file counts from arguments specified in file_args --------------
    file_counts_args <- NULL
    if (!is.null(file_args)) {
      fa <- intersect(as.character(file_args), names(.anames))

      if (length(fa) > 0L) {
        fc <- lapply(fa, function(arg) {
          val <- .anames[[arg]]
          if (is.null(val)) return(NA_integer_)

          dirs <- as.character(val)
          counts <- vapply(dirs, function(d) {
            d_norm <- normalizePath(d, mustWork = FALSE)
            if (!dir.exists(d_norm)) return(0L)
            length(list.files(d_norm))
          }, integer(1L))

          sum(counts)
        })

        file_counts_args <- setNames(unlist(fc, use.names = FALSE), fa)
      }
    }

    # ---- file counts from hardcoded literal directories -----------------
    static_counts_lit <- NULL
    if (length(static_dirs_lit) > 0L) {
      static_counts_lit <- vapply(static_dirs_lit, function(d) {
        d_norm <- normalizePath(d, mustWork = FALSE)
        if (!dir.exists(d_norm)) return(0L)
        length(list.files(d_norm))
      }, integer(1L))
      names(static_counts_lit) <- static_dirs_lit
    }

    # ---- file counts from global vars used as dir()/list.files() paths --
    static_counts_sym <- NULL
    if (length(static_dirs_sym) > 0L) {
      static_counts_sym <- vapply(static_dirs_sym, function(vname) {
        val <- tryCatch(
          get(vname, envir = parent.frame(), inherits = TRUE),
          error = function(e) NULL
        )
        if (is.null(val)) return(NA_integer_)

        dirs <- as.character(val)
        counts <- vapply(dirs, function(d) {
          d_norm <- normalizePath(d, mustWork = FALSE)
          if (!dir.exists(d_norm)) return(0L)
          length(list.files(d_norm))
        }, integer(1L))

        sum(counts)
      }, integer(1L))
      names(static_counts_sym) <- paste0("sym:", static_dirs_sym)
    }

    # ---- combine all counts into a single file_counts vector ------------
    all_counts <- c(file_counts_args, static_counts_lit, static_counts_sym)
    file_counts <- if (length(all_counts)) {
      # merge by name in case something repeats
      tapply(all_counts, names(all_counts), sum)
    } else {
      NULL
    }

    # ---- compute hash & output path -------------------------------------
    hashlist  <- list(
      anames      = .anames,
      body        = .fbody,
      file_counts = file_counts,   # depends on arg + static dir file counts
      pkgs        = .pkg_deps      # NEW: package dependencies (name + version)
    )

    args_hash <- digest::digest(hashlist, algo = "md5")
    message(args_hash)

    outfile <- file.path(
      cache_dir,
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

    # ---- caching logic (as before) --------------------------------------
    if (.load && file.exists(outfile)) {
      message(paste0(fname, ": Returning loaded data ..."))
      message(outfile)
      .cacheR_read(outfile)$dat
    } else {
      message(paste0(fname, ": Running function ..."))
      dat <- f(...)

      .cacheR_write(
        list(
          dat         = dat,
          args        = .anames,
          body        = .fbody,
          file_counts = file_counts,   # extra metadata
          pkgs        = .pkg_deps      # NEW: stored in cache metadata
        ),
        outfile
      )
      dat
    }
  }
}



# --- helper: find literal directory strings in the function body ------
.find_path_specs <- function(expr) {
  literal_dirs <- character()
  symbol_dirs  <- character()

  walk <- function(e) {
    if (is.call(e)) {
      fname <- as.character(e[[1]])[1]

      if (fname %in% c("list.files", "dir", "list.dirs")) {
        args      <- as.list(e)[-1]
        arg_names <- names(args)
        target    <- NULL

        # named 'path' argument if present, else first positional
        if (!is.null(arg_names) && "path" %in% arg_names) {
          target <- args[["path"]]
        } else if (length(args) >= 1) {
          target <- args[[1]]
        }

        if (!is.null(target)) {
          if (is.character(target) && length(target) == 1L) {
            literal_dirs <<- c(literal_dirs, target)
          } else if (is.name(target)) {
            symbol_dirs <<- c(symbol_dirs, as.character(target))
          }
        }
      }

      # recurse into call arguments
      for (el in as.list(e)) walk(el)
    }
  }

  walk(expr)

  list(
    literals = unique(literal_dirs),
    symbols  = unique(symbol_dirs)
  )
}


.find_package_deps <- function(fun) {
  globs <- codetools::findGlobals(fun, merge = FALSE)

  fun_names <- unique(globs$functions)

  pkgs <- unique(unlist(lapply(fun_names, function(sym) {
    ga <- getAnywhere(sym)

    if (!length(ga$where)) return(NULL)

    ns <- ga$where[grepl("^package:", ga$where)]
    if (!length(ns)) return(NULL)

    sub("^package:", "", ns)
  })))

  # optional: ignore base-ish stuff
  pkgs <- setdiff(pkgs, c("base", "stats", "utils", "graphics", "grDevices", "methods"))

  if (!length(pkgs)) return(NULL)

  versions <- vapply(pkgs, function(p) as.character(utils::packageVersion(p)), character(1))

  data.frame(
    package = pkgs,
    version = versions,
    stringsAsFactors = FALSE
  )
}