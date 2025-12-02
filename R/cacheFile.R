#' Create a disk-backed caching decorator for a function
#'
#' `cacheFile()` creates a decorator that caches function results to files
#' in `cache_dir`. It tracks arguments, the function body, and the state (mtime)
#' of any directories passed as arguments.
#'
#' @param cache_dir   Directory where cache files will be stored.
#' @param backend     Storage backend: "rds" (default) or "qs".
#' @param file_args   Optional character vector of argument names to strictly treat as paths.
#' @param ignore_args Character vector of argument names to exclude from the hash.
#' @param algo        Hashing algorithm to use (default "xxhash64").
#'
#' @return A decorator object usable via `%@%`.
#' @export
cacheFile <- function(cache_dir   = NULL,
                      backend     = getOption("cacheR.backend", "rds"),
                      file_args   = NULL,
                      ignore_args = NULL,
                      algo        = "xxhash64") decorator %@% function(f) {

  ## -----------------------------------------------------------------------
  ## 1. Setup Cache Directory
  ## -----------------------------------------------------------------------
  if (is.null(cache_dir)) {
    cache_dir <- cacheR_default_dir()
  } else if (!file.exists(cache_dir)) {
    stop("the designated cache_dir does not exist")
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  backend   <- match.arg(backend, c("rds", "qs"))


  ## -----------------------------------------------------------------------
  ## 2. Static Metadata Analysis (Run once at decoration time)
  ## -----------------------------------------------------------------------
  fbody           <- lapply(as.list(body(f)), as.character)
  path_specs      <- .find_path_specs(body(f))
  static_dirs_lit <- path_specs$literals
  static_dirs_sym <- path_specs$symbols
  pkg_deps        <- .find_package_deps(f)


  ## -----------------------------------------------------------------------
  ## 3. Helper: Directory State Hasher
  ##    Calculates hash of file mtimes to detect content changes.
  ## -----------------------------------------------------------------------
  .get_dir_hash <- function(path) {
    p <- normalizePath(path, mustWork = FALSE)
    if (!dir.exists(p)) return(NA_character_)

    files <- list.files(p, recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) return("empty_dir")

    # Hash modification times (fast & effective for caching)
    mtimes <- file.info(files)$mtime
    digest::digest(mtimes, algo = algo)
  }


  ## =======================================================================
  ## 4. The Runtime Wrapper
  ##    Replaces the original function 'f'
  ## =======================================================================
  function(..., .load = TRUE, .pkg_deps = pkg_deps) {

    invoke_env <- parent.frame()
    full_call  <- match.call(expand.dots = TRUE)

    ## -- Step A: Safe Function Name Extraction ----------------------------
    #  Robustly determine the function name for the cache filename.
    #  Handles symbols (f), namespaced calls (pkg::f), or strings.
    fname_raw <- full_call[[1]]
    fname     <- "anon"

    if (is.symbol(fname_raw)) {
      fname <- as.character(fname_raw)
    } else if (is.call(fname_raw) && length(fname_raw) == 3 &&
               as.character(fname_raw[[1]]) %in% c("::", ":::")) {
      fname <- as.character(fname_raw[[3]])
    } else if (is.character(fname_raw)) {
      fname <- fname_raw
    }

    ## -- Step B: Clean the Call & Match Arguments -------------------------
    call_f_list <- as.list(full_call)

    # Strip internal wrapper arguments (.load, .pkg_deps)
    if (!is.null(names(call_f_list))) {
      keep     <- !names(call_f_list) %in% c(".load", ".pkg_deps")
      keep[1L] <- TRUE  # Always keep the function symbol
      call_f_list <- call_f_list[keep]
    }

    call_f_list[[1]] <- quote(f)
    call_f           <- as.call(call_f_list)

    # Align arguments to function signature
    call_matched  <- match.call(definition = f, call = call_f, expand.dots = TRUE)
    args_for_hash <- as.list(call_matched)[-1]


    ## -- Step C: Inject Defaults & Filter Ignored Args --------------------
    f_formals <- formals(f)
    f_formals <- f_formals[names(f_formals) != "..."]

    # Fill in default values not explicitly passed
    missing <- setdiff(names(f_formals), names(args_for_hash))
    if (length(missing) > 0) {
      args_for_hash <- c(args_for_hash, f_formals[missing])
    }

    # Remove arguments excluded by user (e.g. verbose=TRUE)
    if (!is.null(ignore_args)) {
      args_for_hash <- args_for_hash[!names(args_for_hash) %in% ignore_args]
    }

    # Sort to ensure f(a=1, b=2) == f(b=2, a=1)
    if (length(args_for_hash) > 0) {
      args_for_hash <- args_for_hash[order(names(args_for_hash))]
    }


    ## -- Step D: Dynamic Argument Scanning --------------------------------
    #  Check every argument; if it is a directory, include its state in hash.
    dir_hashes_args <- character()

    if (length(args_for_hash)) {
      dh <- lapply(args_for_hash, function(expr) {
        val <- tryCatch(eval(expr, envir = invoke_env), error = function(e) NULL)

        # Valid path check
        if (is.null(val) || !is.character(val) || !length(val)) return(NULL)
        dirs <- normalizePath(val, mustWork = FALSE)
        dirs <- dirs[dir.exists(dirs)]
        if (!length(dirs)) return(NULL)

        # Calculate mtime hash for each directory found
        vapply(unique(dirs), .get_dir_hash, character(1L))
      })

      dh <- Filter(Negate(is.null), dh)
      if (length(dh)) dir_hashes_args <- unlist(dh)
    }


    ## -- Step E: Static Path Scanning -------------------------------------
    #  1. Literal paths found in code: list.files("data/raw")
    static_hashes_lit <- character()
    if (length(static_dirs_lit)) {
      static_hashes_lit <- vapply(static_dirs_lit, .get_dir_hash, character(1L))
      names(static_hashes_lit) <- static_dirs_lit
    }

    #  2. Symbolic paths found in code: list.files(MY_GLOBAL_DIR)
    static_hashes_sym <- character()
    if (length(static_dirs_sym)) {
      static_hashes_sym <- vapply(static_dirs_sym, function(v) {
        val <- tryCatch(get(v, envir = invoke_env), error = function(e) NULL)
        if (!is.character(val)) return(NA_character_)

        # A variable might contain multiple paths; aggregate their hashes
        digest::digest(vapply(val, .get_dir_hash, character(1L)), algo = algo)
      }, character(1L))
      names(static_hashes_sym) <- paste0("sym:", static_dirs_sym)
    }


    ## -- Step F: Build Master Hash ----------------------------------------
    all_dir_hashes <- c(dir_hashes_args, static_hashes_lit, static_hashes_sym)
    all_dir_hashes <- all_dir_hashes[!is.na(all_dir_hashes)]

    if (length(all_dir_hashes)) {
      all_dir_hashes <- all_dir_hashes[order(names(all_dir_hashes))]
    }

    hashlist <- list(
      call       = args_for_hash,
      body       = fbody,
      dir_states = all_dir_hashes,
      pkgs       = .pkg_deps
    )

    args_hash <- digest::digest(hashlist, algo = algo)
    outfile   <- file.path(cache_dir, paste(fname, args_hash, backend, sep = "."))


    ## -- Step G: Register Node & Execute ----------------------------------
    node_id <- paste(fname, args_hash, sep = ":")
    .cacheTree_register_node(node_id, fname, args_hash, outfile)

    # Update call stack for dependency tracking
    .cacheTree_env$call_stack <- c(.cacheTree_env$call_stack, node_id)
    on.exit({
      .cacheTree_env$call_stack <- head(.cacheTree_env$call_stack, -1L)
    }, add = TRUE)

    # -- Cache Hit or Miss Logic --
    if (.load && file.exists(outfile)) {
      .cacheR_load(path = outfile)$dat
    } else {
      dat <- f(...)
      .cacheR_save(
        list(dat        = dat,
             args       = args_for_hash,
             body       = fbody,
             dir_states = all_dir_hashes,
             pkgs       = .pkg_deps),
        outfile,
        backend
      )
      dat
    }
  }
}


#' @importFrom digest digest
#' @importFrom utils packageVersion
#' @importFrom codetools findGlobals
NULL


## -------------------------------------------------------------------------
## Helper: Find Path Dependencies (.find_path_specs)
## Scans function body for calls to 'list.files', 'dir', etc.
## -------------------------------------------------------------------------
.find_path_specs <- function(expr) {
  literal_dirs <- character()
  symbol_dirs  <- character()
  target_funs  <- c("list.files", "dir", "list.dirs")

  recurse <- function(sub_node, collect = FALSE) {
    # If collecting, grab the string or symbol
    if (collect && is.symbol(sub_node)) {
      symbol_dirs <<- c(symbol_dirs, as.character(sub_node))
      return(invisible())
    }
    if (collect && is.character(sub_node)) {
      literal_dirs <<- c(literal_dirs, as.character(sub_node))
      return(invisible())
    }
    if (!is.language(sub_node)) return(invisible())

    if (is.call(sub_node)) {
      head  <- sub_node[[1L]]
      fname <- NULL

      # Detect function name (handle pkg::fun)
      if (is.symbol(head)) {
        fname <- as.character(head)
      } else if (is.call(head)) {
        hp <- as.character(head)
        if (length(hp) >= 3 && hp[1] %in% c("::", ":::")) fname <- hp[3]
      }

      is_target <- !is.null(fname) && fname %in% target_funs

      if (is_target && !collect) {
        # Found target function; find 'path' arg and recurse with collect=TRUE
        args      <- as.list(sub_node)[-1L]
        arg_names <- names(args)
        target    <- if ("path" %in% arg_names) args[["path"]] else if (length(args) >= 1) args[[1L]] else NULL

        if (!is.null(target)) recurse(target, collect = TRUE)

        # Recurse other args without collecting
        if (length(args)) {
          for (j in seq_along(args)) {
            if (!is.null(target) && identical(args[[j]], target)) next
            recurse(args[[j]], collect = FALSE)
          }
        }
      } else {
        # Standard recursion
        for (j in seq_along(sub_node)) recurse(sub_node[[j]], collect = collect)
      }
    } else if (is.pairlist(sub_node)) {
      for (j in seq_along(sub_node)) recurse(sub_node[[j]], collect = collect)
    }
  }

  tryCatch(recurse(expr), error = function(e) warning(".find_path_specs failed: ", conditionMessage(e)))
  list(literals = unique(literal_dirs), symbols = unique(symbol_dirs))
}


## -------------------------------------------------------------------------
## Helper: Find Package Dependencies (.find_package_deps)
## Scans body for 'pkg::fun' calls and uses codetools for globals.
## -------------------------------------------------------------------------
.find_package_deps <- function(fun) {
  stopifnot(is.function(fun))
  pkgs       <- character()
  expr_stack <- list(body(fun))

  ## 1. AST Traversal for Explicit Calls
  while (length(expr_stack)) {
    node       <- expr_stack[[1L]]
    expr_stack <- expr_stack[-1L]

    if (is.call(node)) {
      head <- node[[1L]]
      # Detect pkg::fun
      if (identical(head, quote(`::`)) || identical(head, quote(`:::`))) {
        pkgs <- c(pkgs, as.character(node[[2L]])[1L])
      }
      children <- as.list(node)
      # Filter missing args (quote(expr=)) to prevent crash
      children <- children[vapply(children, function(x) !identical(x, quote(expr =)), logical(1))]
      if (length(children)) expr_stack <- c(children, expr_stack)

    } else if (is.pairlist(node) || is.expression(node)) {
      children <- as.list(node)
      # Filter missing args
      children <- children[vapply(children, function(x) !identical(x, quote(expr =)), logical(1))]
      if (length(children)) expr_stack <- c(children, expr_stack)
    }
  }

  ## 2. Codetools for Implicit Globals
  if (requireNamespace("codetools", quietly = TRUE)) {
    globs <- codetools::findGlobals(fun, merge = FALSE)
    if (length(globs$functions)) {
      pkgs_globals <- unlist(lapply(globs$functions, function(sym) {
        ga <- getAnywhere(sym)
        if (!length(ga$where)) return(NULL)
        ns <- ga$where[grepl("^package:", ga$where)]
        if (!length(ns)) return(NULL)
        sub("^package:", "", ns)
      }), use.names = FALSE)
      if (length(pkgs_globals)) pkgs <- c(pkgs, pkgs_globals)
    }
  }

  ## 3. Formatting & Versions
  if (!length(pkgs)) return(NULL)
  pkgs <- unique(pkgs)
  pkgs <- setdiff(pkgs, c("base", "stats", "utils", "graphics", "grDevices", "methods"))

  if (!length(pkgs)) return(NULL)

  versions <- vapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }, character(1L))

  data.frame(package = pkgs, version = versions, stringsAsFactors = FALSE)
}