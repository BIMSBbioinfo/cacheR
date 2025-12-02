#' Create a disk-backed caching decorator for a function
#'
#' `cacheFile()` creates a decorator that caches function results to files
#' in `cache_dir`, keyed by the function body and arguments. It also records
#' parent-child relationships between cached calls so that a "cache tree"
#' can be reconstructed.
#'
#' @param cache_dir Directory where cache files will be stored.
#' @param backend   "rds" or "qs".
#' @param file_args Character vector of argument names that should be treated
#'                  as directory paths whose contents must be monitored.
#'
#' @return A decorator object usable via `%@%`.
#' @export
cacheFile <- function(
    cache_dir = NULL,
    backend   = getOption("cacheR.backend", "rds"),
    file_args = NULL  # optional manual file-args
) decorator %@% function(f) {

  ## ---- setup cache directory ----
  if (is.null(cache_dir)) {
    cache_dir <- cacheR_default_dir()
  } else if (!file.exists(cache_dir)) {
    stop("the designated cache_dir does not exist")
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  backend <- match.arg(backend, c("rds", "qs"))

  ## ---- static metadata from f ----------------------------------------
  fbody      <- lapply(as.list(body(f)), as.character)
  path_specs <- .find_path_specs(body(f))
  static_dirs_lit <- path_specs$literals
  static_dirs_sym <- path_specs$symbols   # names used as paths in body

  pkg_deps <- .find_package_deps(f)       # may be NULL


  ## ===================================================================
  ## Wrapper
  ## ===================================================================
  function(..., .load = TRUE, .pkg_deps = pkg_deps) {
    
    # Capture the environment of the caller immediately.
    invoke_env <- parent.frame()

    ## (1) capture wrapper call (dots expanded) -------------------------
    full_call <- match.call(expand.dots = TRUE)
    fname     <- full_call[[1]]

    ## (2) strip internal args and retarget to f ------------------------
    call_f_list <- as.list(full_call)

    if (!is.null(names(call_f_list))) {
      keep <- !names(call_f_list) %in% c(".load", ".pkg_deps")
      keep[1L] <- TRUE  # keep function symbol
      call_f_list <- call_f_list[keep]
    }

    call_f_list[[1]] <- quote(f)    # call f(...)
    call_f <- as.call(call_f_list)

    ## (3) match call to f's formals ------------------------------------
    call_matched <- match.call(definition = f, call = call_f, expand.dots = TRUE)
    args_for_hash <- as.list(call_matched)[-1]
    
    # --- NEW: Inject Default Arguments ---
    # This ensures f() hashes identically to f(x=default)
    f_formals <- formals(f)
    f_formals <- f_formals[names(f_formals) != "..."]
    
    missing_args <- setdiff(names(f_formals), names(args_for_hash))
    if (length(missing_args) > 0) {
      defaults <- f_formals[missing_args]
      args_for_hash <- c(args_for_hash, defaults)
    }
    
    # Sort by name to guarantee consistent hashing order
    if (length(args_for_hash) > 0) {
      args_for_hash <- args_for_hash[order(names(args_for_hash))]
    }

    # argument names actually used in this call (from the expanded hash list)
    arg_exprs <- args_for_hash
    arg_names_call <- names(arg_exprs)


    ## ================================================================
    ## (4) ARGUMENT-BASED PATH COUNTS (runtime detection)
    ## ================================================================
    # We now scan ALL arguments that look like directories.
    # 'file_args' can still be passed for documentation, but we don't
    # strictly filter by it anymore.
    
    file_counts_args <- NULL
    
    if (length(arg_exprs)) {
      fc <- lapply(seq_along(arg_exprs), function(i) {
        expr <- arg_exprs[[i]]
        
        # Evaluate in the captured invocation environment
        val  <- tryCatch(
          eval(expr, envir = invoke_env),
          error = function(e) NULL 
        )
        if (is.null(val)) return(integer(0L))

        # Only character-like values can be paths
        if (!is.character(val)) return(integer(0L))

        dirs <- as.character(val)
        if (!length(dirs)) return(integer(0L))

        # Check validity
        dirs_norm <- normalizePath(dirs, mustWork = FALSE)
        ok <- dir.exists(dirs_norm)
        if (!any(ok)) return(integer(0L))
        
        dirs_norm <- dirs_norm[ok]
        u_dirs <- unique(dirs_norm)

        counts <- vapply(u_dirs, function(d) {
          length(list.files(d))
        }, integer(1L))
        
        names(counts) <- u_dirs
        counts
      })

      if (length(fc)) {
        file_counts_args <- unlist(fc, use.names = TRUE)
      }
    }


    ## ================================================================
    ## (5) STATIC LITERAL PATH COUNTS
    ## ================================================================
    static_counts_lit <- NULL
    if (length(static_dirs_lit) > 0L) {
      static_counts_lit <- vapply(static_dirs_lit, function(d) {
        p <- normalizePath(d, mustWork = FALSE)
        if (!dir.exists(p)) return(0L)
        length(list.files(p))
      }, integer(1L))
      names(static_counts_lit) <- static_dirs_lit
    }


    ## ================================================================
    ## (6) STATIC GLOBAL SYMBOL PATH COUNTS
    ## ================================================================
    static_counts_sym <- NULL
    if (length(static_dirs_sym) > 0L) {
      static_counts_sym <- vapply(static_dirs_sym, function(vname) {
        val <- tryCatch(
          get(vname, envir = invoke_env, inherits = TRUE),
          error = function(e) NULL
        )
        if (is.null(val)) return(NA_integer_)
        
        # Ensure it is a character vector before casting.
        if (!is.character(val)) return(NA_integer_)

        dirs <- as.character(val)

        counts <- vapply(dirs, function(d) {
          p <- normalizePath(d, mustWork = FALSE)
          if (!dir.exists(p)) return(0L)
          length(list.files(p))
        }, integer(1L))

        sum(counts)
      }, integer(1L))
      names(static_counts_sym) <- paste0("sym:", static_dirs_sym)
    }


    ## ================================================================
    ## (7) MERGE ALL FILE COUNTS
    ## ================================================================
    all_counts <- c(file_counts_args, static_counts_lit, static_counts_sym)
    all_counts <- all_counts[!is.na(all_counts)]

    file_counts <- if (length(all_counts)) {
      tapply(all_counts, names(all_counts), sum)
    } else {
      NULL
    }


    ## ================================================================
    ## (8) BUILD HASH & OUTFILE
    ## ================================================================
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


    ## ================================================================
    ## (9) REGISTER NODE IN CACHE TREE
    ## ================================================================
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


    ## ================================================================
    ## (10) LOAD OR EXECUTE + SAVE
    ## ================================================================
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

#' @importFrom digest digest
#' @importFrom utils packageVersion
#' @importFrom codetools findGlobals
NULL

# --- helper: find path dependencies of a function -------------------
.find_path_specs <- function(expr) {
  literal_dirs <- character()
  symbol_dirs  <- character()

  target_funs <- c("list.files", "dir", "list.dirs")

  recurse <- function(sub_node, collect = FALSE) {
    if (collect && is.symbol(sub_node)) {
      symbol_dirs <<- c(symbol_dirs, as.character(sub_node))
      return(invisible())
    }
    if (collect && is.character(sub_node)) {
      literal_dirs <<- c(literal_dirs, as.character(sub_node))
      return(invisible())
    }

    if (!is.language(sub_node)) {
      return(invisible())
    }

    if (is.call(sub_node)) {
      head <- sub_node[[1L]]
      fname <- NULL

      if (is.symbol(head)) {
        fname <- as.character(head)
      } else if (is.call(head)) {
        head_parts <- as.character(head)
        if (length(head_parts) >= 3L &&
            head_parts[1L] %in% c("::", ":::")) {
          fname <- head_parts[3L]
        }
      }

      is_target <- !is.null(fname) && fname %in% target_funs

      if (is_target && !collect) {
        args      <- as.list(sub_node)[-1L]
        arg_names <- names(args)
        target    <- NULL

        if (!is.null(arg_names) && "path" %in% arg_names) {
          target <- args[["path"]]
        } else if (length(args) >= 1L) {
          target <- args[[1L]]
        }

        if (!is.null(target)) {
          recurse(target, collect = TRUE)
        }

        if (length(args)) {
          for (j in seq_along(args)) {
            if (!is.null(target) && identical(args[[j]], target)) next
            recurse(args[[j]], collect = FALSE)
          }
        }
      } else {
        for (j in seq_along(sub_node)) {
          recurse(sub_node[[j]], collect = collect)
        }
      }
    } else if (is.pairlist(sub_node)) {
      for (j in seq_along(sub_node)) {
        recurse(sub_node[[j]], collect = collect)
      }
    }
  }

  tryCatch(
    recurse(expr, collect = FALSE),
    error = function(e) {
      warning(".find_path_specs failed: ", conditionMessage(e))
    }
  )

  list(
    literals = unique(literal_dirs),
    symbols  = unique(symbol_dirs)
  )
}

# --- helper: find package dependencies of a function -------------------
.find_package_deps <- function(fun) {
  stopifnot(is.function(fun))

  pkgs <- character()

  ## ---- 1) Explicit pkg::fun / pkg:::fun calls in the body ----------

  expr_stack <- list(body(fun))
  
  while (length(expr_stack)) {
    node       <- expr_stack[[1L]]
    expr_stack <- expr_stack[-1L]

    if (is.call(node)) {
      head <- node[[1L]]

      # detect pkg::fun or pkg:::fun
      if (identical(head, quote(`::`)) || identical(head, quote(`:::`))) {
        pkg_sym <- node[[2L]]
        pkg <- as.character(pkg_sym)[1L]
        pkgs <- c(pkgs, pkg)
      }

      # push all children of this call onto the stack
      children <- as.list(node)
      
      # --- FIX: Filter out missing arguments ---
      valid <- vapply(children, function(x) !identical(x, quote(expr=)), logical(1))
      children <- children[valid]

      if (length(children)) {
        expr_stack <- c(children, expr_stack)
      }

    } else if (is.pairlist(node) || is.expression(node)) {
      # Recurse into pairlists / expression objects
      children <- as.list(node)

      # --- FIX: Filter out missing arguments ---
      valid <- vapply(children, function(x) !identical(x, quote(expr=)), logical(1))
      children <- children[valid]
      
      if (length(children)) {
        expr_stack <- c(children, expr_stack)
      }
    }
  }

  ## ---- 2) Unqualified functions via codetools::findGlobals ---------
  if (requireNamespace("codetools", quietly = TRUE)) {
    globs <- codetools::findGlobals(fun, merge = FALSE)
    fun_names <- unique(globs$functions)

    if (length(fun_names)) {
      pkgs_globals <- unlist(
        lapply(fun_names, function(sym) {
          ga <- getAnywhere(sym)
          if (!length(ga$where)) return(NULL)

          ns <- ga$where[grepl("^package:", ga$where)]
          if (!length(ns)) return(NULL)

          sub("^package:", "", ns)
        }),
        use.names = FALSE
      )

      if (length(pkgs_globals)) {
        pkgs <- c(pkgs, pkgs_globals)
      }
    }
  }

  ## ---- 3) Clean, drop base-ish, and attach versions ----------------
  if (!length(pkgs)) return(NULL)

  pkgs <- unique(pkgs)

  pkgs <- setdiff(pkgs, c(
    "base", "stats", "utils",
    "graphics", "grDevices", "methods"
  ))
  
  # --- FIX: Handle empty package list after filtering ---
  if (!length(pkgs)) return(NULL)

  versions <- vapply(
    pkgs,
    function(p) {
      if (requireNamespace(p, quietly = TRUE)) {
        as.character(utils::packageVersion(p))
      } else {
        NA_character_
      }
    },
    character(1L)
  )

  data.frame(
    package = pkgs,
    version = versions,
    stringsAsFactors = FALSE
  )
}