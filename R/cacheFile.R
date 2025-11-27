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
    file_args = NULL  # optional: names of args that are directories
) decorator %@% function(f) {

  ## ---- setup cache directory ----
  if (is.null(cache_dir)) {
    cache_dir <- cacheR_default_dir()
  } else if (!file.exists(cache_dir)) {
    stop("the designated cache_dir does not exist")
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  backend <- match.arg(backend, c("rds", "qs"))

  ## ---- function metadata (static) -----------------------------------
  f_formals <- names(formals(f))
  fbody     <- lapply(as.list(body(f)), as.character)

  path_specs      <- .find_path_specs(body(f))
  static_dirs_lit <- path_specs$literals
  static_dirs_sym <- path_specs$symbols

  ## auto-detect which symbols are arguments vs globals
  auto_file_args  <- intersect(static_dirs_sym, f_formals)
  global_syms     <- setdiff(static_dirs_sym, auto_file_args)

  if (is.null(file_args)) {
    effective_file_args <- auto_file_args
  } else {
    effective_file_args <- unique(c(as.character(file_args), auto_file_args))
  }

  pkg_deps <- .find_package_deps(f)   # may be NULL for base-only functions

  ## ===================================================================
  ## Wrapper
  ## ===================================================================
  function(..., .load = TRUE, .pkg_deps = pkg_deps) {

    ## ----------------------------------------------------------
    ## (1) capture wrapper call, with dots expanded
    ## ----------------------------------------------------------
    full_call <- match.call(expand.dots = TRUE)   # e.g. cached_fun(input_dir, .load = FALSE)
    fname     <- full_call[[1]]

    ## ----------------------------------------------------------
    ## (2) drop internal args (.load, .pkg_deps) and retarget to f
    ## ----------------------------------------------------------
    call_f_list <- as.list(full_call)

    if (!is.null(names(call_f_list))) {
      keep <- !names(call_f_list) %in% c(".load", ".pkg_deps")
      keep[1L] <- TRUE  # first element is the function symbol, keep it
      call_f_list <- call_f_list[keep]
    }

    # retarget function symbol to 'f'
    call_f_list[[1]] <- quote(f)
    call_f <- as.call(call_f_list)

    ## ----------------------------------------------------------
    ## (3) match against f's formals
    ##     → positional args become named (e.g. path = input_dir)
    ## ----------------------------------------------------------
    call_matched <- match.call(definition = f, call = call_f, expand.dots = TRUE)

    # unevaluated args used for hashing
    args_for_hash <- as.list(call_matched)[-1]

    ## ----------------------------------------------------------
    ## (4) dynamic file counts from argument-based dirs
    ## ----------------------------------------------------------
    file_counts_args <- NULL
    if (length(effective_file_args)) {
      fa <- unique(effective_file_args)

      fc <- lapply(fa, function(argname) {
        if (!argname %in% names(call_matched)) return(NA_integer_)

        expr <- call_matched[[argname]]
        val  <- tryCatch(
          eval(expr, envir = parent.frame()),
          error = function(e) NULL
        )
        if (is.null(val)) return(NA_integer_)

        dirs <- as.character(val)
        counts <- vapply(dirs, function(d) {
          p <- normalizePath(d, mustWork = FALSE)
          if (!dir.exists(p)) return(0L)
          length(list.files(p))
        }, integer(1L))

        sum(counts)
      })

      names(fc) <- fa
      file_counts_args <- unlist(fc, use.names = TRUE)
    }

    ## ----------------------------------------------------------
    ## (5) static literal path counts
    ## ----------------------------------------------------------
    static_counts_lit <- NULL
    if (length(static_dirs_lit) > 0L) {
      static_counts_lit <- vapply(static_dirs_lit, function(d) {
        p <- normalizePath(d, mustWork = FALSE)
        if (!dir.exists(p)) return(0L)
        length(list.files(p))
      }, integer(1L))
      names(static_counts_lit) <- static_dirs_lit
    }

    ## ----------------------------------------------------------
    ## (6) static global-variable dir counts
    ## ----------------------------------------------------------
    static_counts_sym <- NULL
    if (length(global_syms) > 0L) {
      static_counts_sym <- vapply(global_syms, function(vname) {
        val <- tryCatch(
          get(vname, envir = parent.frame(), inherits = TRUE),
          error = function(e) NULL
        )
        if (is.null(val)) return(NA_integer_)

        dirs <- as.character(val)
        counts <- vapply(dirs, function(d) {
          p <- normalizePath(d, mustWork = FALSE)
          if (!dir.exists(p)) return(0L)
          length(list.files(p))
        }, integer(1L))

        sum(counts)
      }, integer(1L))
      names(static_counts_sym) <- paste0("sym:", global_syms)
    }

    ## ----------------------------------------------------------
    ## (7) merge all file counts
    ## ----------------------------------------------------------
    all_counts <- c(file_counts_args, static_counts_lit, static_counts_sym)
    file_counts <- if (length(all_counts)) {
      tapply(all_counts, names(all_counts), sum)
    } else {
      NULL
    }

    ## ----------------------------------------------------------
    ## (8) build hash & outfile path
    ## ----------------------------------------------------------
    hashlist <- list(
      call        = args_for_hash,
      body        = fbody,
      file_counts = file_counts,
      pkgs        = .pkg_deps
    )

    args_hash <- digest::digest(hashlist, algo = "md5")

    outfile <- file.path(
      cache_dir,
      paste(as.character(fname), args_hash, backend, sep = ".")
    )

    ## ----------------------------------------------------------
    ## (9) register in cache tree
    ## ----------------------------------------------------------
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

    ## ----------------------------------------------------------
    ## (10) load or run + save
    ## ----------------------------------------------------------
    if (.load && file.exists(outfile)) {
      .cacheR_load(path = outfile)$dat
    } else {
      dat <- f(...)

      .cacheR_save(
        object = list(
          dat         = dat,
          args        = args_for_hash,
          body        = fbody,
          file_counts = file_counts,
          pkgs        = .pkg_deps
        ),
        path    = outfile,
        backend = backend
      )
      dat
    }
  }
}



## ============================================================
## Helpers (top-level)
## ============================================================

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

# --- helper: find package dependencies of a function -------------------
.find_package_deps <- function(fun) {
  expr <- body(fun)
  pkgs <- character()

  walk <- function(e) {
    if (is.call(e)) {
      funsym <- e[[1]]

      # detect pkg::fun or pkg:::fun
      if (identical(funsym, quote(`::`)) || identical(funsym, quote(`:::`))) {
        pkg <- as.character(e[[2]])[1]
        pkgs <<- c(pkgs, pkg)
      }

      for (el in as.list(e)) walk(el)
    }
  }

  walk(expr)

  pkgs <- unique(pkgs)
  if (!length(pkgs)) return(NULL)

  versions <- vapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      NA_character_
    }
  }, character(1L))

  data.frame(
    package = pkgs,
    version = versions,
    stringsAsFactors = FALSE
  )
}

