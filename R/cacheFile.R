# -------------------------------------------------------------------------
# 1. Global State (Package Level)
# -------------------------------------------------------------------------

#' File State Cache
#' 
#' An internal environment used to memoize file hashes based on their 
#' size and modification time (mtime). This avoids re-reading large files 
#' if their metadata hasn't changed.
#' @keywords internal
.file_state_cache <- new.env(parent = emptyenv())

# -------------------------------------------------------------------------
# 2. Helper Functions
# -------------------------------------------------------------------------

#' Probabilistic File Hashing
#'
#' Computes a hash of a file by reading specific blocks rather than the 
#' entire content. This is significantly faster for large files (>100MB).
#' It reads the header, the footer, and a deterministic set of random blocks 
#' derived from the file size and name.
#'
#' @param path Character string. Path to the file.
#' @param block_size Integer. Size of each block in bytes (default 64KB).
#' @param n_blocks Integer. Number of random blocks to sample.
#' @param algo Character. Hashing algorithm (default "xxhash64").
#' 
#' @return A character string containing the hash, or NA if file is missing.
#' @keywords internal
.probabilistic_file_hash <- function(path, block_size = 64 * 1024, n_blocks = 5, algo = "xxhash64") {
  if (!file.exists(path)) return(NA_character_)
  
  info <- file.info(path)
  size <- info$size
  if (is.na(size)) return(NA_character_)
  
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  
  blocks <- list()
  # 1. Always read the header (first block)
  blocks[[1]] <- readBin(con, "raw", block_size)
  
  if (size > block_size) {
    max_offset <- max(size - block_size, 1)
    
    # 2. Deterministic Random Sampling
    # We temporarily set the RNG seed based on the file path + size.
    # This ensures that for the same file, we always sample the exact same "random" blocks.
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    }
    
    seed_str <- paste0(path, size)
    # Convert string hash to integer for set.seed
    seed_int <- sum(as.integer(charToRaw(digest::digest(seed_str, algo = "crc32"))))
    set.seed(seed_int)
    
    # Read N random intermediate blocks
    for (i in seq_len(n_blocks)) {
      offset <- sample.int(max_offset, 1)
      seek(con, offset, "start")
      blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
    }
    
    # 3. Always read the footer (last block)
    seek(con, max(size - block_size, 0), "start")
    blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  }
  
  bytes <- do.call(c, blocks)
  digest::digest(bytes, algo = algo)
}

#' Fast File Hash with Memoization
#'
#' Checks the file's metadata (size + mtime). If it matches the cached state
#' in `.file_state_cache`, returns the previously computed hash immediately. 
#' Otherwise, re-computes the hash and updates the cache.
#'
#' @param path Character string. Path to the file.
#' @param algo Character. Hashing algorithm.
#' @return The file hash.
#' @keywords internal
.fast_file_hash <- function(path, algo = "xxhash64") {
  info <- file.info(path)
  if (is.na(info$size)) return(NA_character_)
  
  # Create a unique footprint based on size and mtime
  fp <- paste(info$size, unclass(info$mtime), sep = "|")
  
  prev <- .file_state_cache[[path]]
  # Return cached hash if footprint matches
  if (!is.null(prev) && identical(prev$fp, fp)) return(prev$hash)
  
  # Compute fresh hash if metadata changed
  h <- .probabilistic_file_hash(path, algo = algo)
  .file_state_cache[[path]] <- list(fp = fp, hash = h)
  h
}

#' Get Connection Path
#' 
#' Attempts to resolve a file path from a connection object.
#' Supports base file connections and DBI SQLite connections.
#' 
#' @param x An object (potential connection).
#' @return A character string (path) or NULL.
#' @keywords internal
.get_connection_path <- function(x) {
  # 1. Base R file connections
  if (inherits(x, "connection")) {
    # summary(con)$description usually holds the filename for file() connections
    try_desc <- try(summary(x)$description, silent = TRUE)
    if (!inherits(try_desc, "try-error") && is.character(try_desc) && file.exists(try_desc)) {
      return(try_desc)
    }
  }
  
  # 2. SQLite / DBI connections
  # We check for "SQLiteConnection" without strictly requiring the library loaded,
  # though we try to use DBI methods if available.
  if (inherits(x, "SQLiteConnection")) {
     # Try DBI::dbGetInfo
     if (requireNamespace("DBI", quietly = TRUE)) {
       info <- try(DBI::dbGetInfo(x), silent = TRUE)
       if (!inherits(info, "try-error") && is.list(info) && "dbname" %in% names(info)) {
         if (file.exists(info$dbname)) return(info$dbname)
       }
     }
     # Fallback: RSQLite stores dbname in the slot (internal, but stable-ish)
     if (isS4(x) && "dbname" %in% slotNames(x)) {
        p <- slot(x, "dbname")
        if (file.exists(p)) return(p)
     }
  }
  
  return(NULL)
}

#' Extract Paths Recursively
#' 
#' Deeply scans an object (lists, environments, args) to find character strings
#' that represent existing files, or connections pointing to files.
#' 
#' @param x Any R object.
#' @return Character vector of paths.
#' @keywords internal
.extract_paths_recursively <- function(x) {
  paths <- character()
  
  if (is.character(x)) {
    # Filter for valid files/dirs
    valid <- x[file.exists(x) | dir.exists(x)]
    if (length(valid) > 0) paths <- c(paths, valid)
    
  } else if (is.list(x)) {
    # Recursively scan lists
    for (el in x) {
      paths <- c(paths, .extract_paths_recursively(el))
    }
    
  } else if (inherits(x, "connection") || inherits(x, "DBIConnection")) {
    # Check connections
    p <- .get_connection_path(x)
    if (!is.null(p)) paths <- c(paths, p)
  }
  
  return(paths)
}

#' Scan AST for Dependencies (Packages and Options)
#'
#' Recursively scans a function's body (Abstract Syntax Tree) to find:
#' 1. `pkg::fun` calls (Package dependencies).
#' 2. `getOption("literal")` calls (Option dependencies).
#'
#' @param expr An R expression or function body.
#' @return A list with `pkgs` (character) and `opts` (character).
#' @keywords internal
.scan_ast_deps <- function(expr) {
  pkgs <- character()
  opts <- character()
  
  if (is.call(expr)) {
    fn_sym <- expr[[1]]
    
    # 1. Detect pkg::fun or pkg:::fun
    if (is.symbol(fn_sym) && (as.character(fn_sym) %in% c("::", ":::"))) {
      pkg_arg <- expr[[2]]
      pkg <- if (is.symbol(pkg_arg)) as.character(pkg_arg) else as.character(pkg_arg)
      pkgs <- c(pkgs, pkg)
    }
    
    # 2. Detect getOption("literal")
    if (is.symbol(fn_sym) && as.character(fn_sym) == "getOption") {
      # expr[[2]] is the option name
      if (length(expr) >= 2 && (is.character(expr[[2]]) || is.character(try(eval(expr[[2]]), silent=TRUE)))) {
         # We try to grab the literal string. If it's a variable, we might miss it (limitation)
         val <- tryCatch(eval(expr[[2]]), error = function(e) NULL)
         if (is.character(val)) opts <- c(opts, val)
      }
    }
    
    # Recurse into all arguments of the call
    for (i in seq_along(expr)) {
      res <- .scan_ast_deps(expr[[i]])
      pkgs <- c(pkgs, res$pkgs)
      opts <- c(opts, res$opts)
    }
    
  } else if (is.expression(expr) || is.list(expr)) {
    # Recurse into lists or expression blocks
    for (i in seq_along(expr)) {
      res <- .scan_ast_deps(expr[[i]])
      pkgs <- c(pkgs, res$pkgs)
      opts <- c(opts, res$opts)
    }
  }
  
  return(list(pkgs = unique(pkgs), opts = unique(opts)))
}

#' Get Package Versions
#'
#' Retrieves the installed version strings for a list of packages.
#' Returns "NA" if a package is not installed.
#'
#' @param pkgs Character vector of package names.
#' @return Named character vector of versions.
#' @keywords internal
.get_pkg_versions <- function(pkgs) {
  if (length(pkgs) == 0) return(NULL)
  vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NA")
  }, character(1))
}

#' Robust Path Hashing
#'
#' Hashes a file or a directory.
#' - For files: Hashes content (via probabilistic hash).
#' - For directories: Hashes the *structure* (relative paths) AND the content of files.
#'   This ensures renaming a file inside a directory changes the directory hash.
#'
#' @param path Character string.
#' @param file_pattern Regex pattern to filter files in directories.
#' @param algo Hashing algorithm.
#' @return String hash.
#' @keywords internal
.get_path_hash <- function(path, file_pattern = NULL, algo = "xxhash64") {
  if (dir.exists(path)) {
    # Get relative paths to detect structure changes (e.g., renames)
    files <- list.files(path, pattern = file_pattern, recursive = TRUE, full.names = FALSE)
    if (length(files) == 0) return(digest::digest("empty_dir", algo = algo))
    
    files <- sort(files)
    full_paths <- file.path(path, files)
    
    # Hash contents of all files
    hashes <- vapply(full_paths, .fast_file_hash, character(1L), algo = algo)
    
    # Combine Structure (names) + Content (hashes)
    dir_data <- list(relative_paths = files, content_hashes = hashes)
    return(digest::digest(dir_data, algo = algo))
  } 
  if (file.exists(path)) return(.fast_file_hash(path, algo = algo))
  return(NULL)
}

# -------------------------------------------------------------------------
# 3. Main Decorator
# -------------------------------------------------------------------------

#' Create a robust, disk-backed caching decorator
#' 
#' Wraps a function with caching logic. The cache key is fully deterministic 
#' and derived from:
#' 1. The function's code (recursive closure hash).
#' 2. The expressions passed to arguments (source code).
#' 3. The evaluated values of arguments (runtime values).
#' 4. The versions of packages explicitly called (e.g. `pkg::fun`).
#' 5. The content of any files passed as string arguments (recursively).
#' 6. Global options used by the function (detected via AST).
#' 
#' @param cache_dir Directory to store cache files. Defaults to a standard user cache dir.
#' @param backend Serialization backend. "rds" (default) or "qs" (faster, requires qs package).
#' @param ignore_args Character vector of argument names to exclude from the hash (e.g. "verbose").
#' @param file_pattern Regex pattern to filter files when hashing directories.
#' @param env_vars Character vector of environment variables to include in the hash.
#' @param algo Hashing algorithm to use (default "xxhash64").
#' 
#' @return A new function that mimics the original but caches results to disk.
#' @export
cacheFile <- function(cache_dir     = NULL,
                      backend       = getOption("cacheR.backend", "rds"),
                      ignore_args   = NULL,
                      file_pattern  = NULL,
                      env_vars      = NULL,
                      algo          = "xxhash64") decorator %@% function(f) {
  
  # Setup Cache Directory
  if (is.null(cache_dir)) cache_dir <- cacheR_default_dir()
  if (!dir.exists(cache_dir)) {
    tryCatch(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE),
             error = function(e) warning("cacheR: Could not create cache directory."))
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  backend   <- match.arg(backend, c("rds", "qs"))
  
  # 1. Static Analysis (One-time check)
  # Scan AST for package dependencies like `pkg::fun` AND `getOption` usage.
  ast_deps <- .scan_ast_deps(body(f))
  ast_pkgs <- ast_deps$pkgs
  ast_opts <- ast_deps$opts
  
  # -----------------------------------------------------------------------
  # The Wrapper Function
  # -----------------------------------------------------------------------
  function(..., .load = TRUE) {
    invoke_env <- parent.frame()
    full_call  <- match.call(expand.dots = TRUE)
    
    # Determine a safe filename prefix from the function name
    fname_raw <- full_call[[1]]
    fname     <- "anon"
    if (is.symbol(fname_raw)) fname <- as.character(fname_raw)
    else if (is.character(fname_raw)) fname <- fname_raw
    fname <- gsub("[^a-zA-Z0-9_]", "_", fname)
    if (nchar(fname) > 50) fname <- substring(fname, 1, 50)
    
    # ---------------------------------------------------------- #
    # A. Parse and Capture Arguments (Expressions AND Values)
    # ---------------------------------------------------------- #
    
    # 1. Capture Expressions using match.call
    call_matched <- match.call(definition = f, expand.dots = TRUE)
    args_exprs   <- as.list(call_matched)[-1] # Remove function name
    
    # 2. Capture Values
    args_values <- lapply(args_exprs, function(expr) {
      eval(expr, envir = invoke_env)
    })
    
    # [NORMALIZATION STEP]
    f_defs <- formals(f)
    f_defs <- f_defs[names(f_defs) != "..."]
    
    eval_env <- new.env(parent = invoke_env)
    
    if (!is.null(names(args_values))) {
       named_indices <- nzchar(names(args_values))
       if (any(named_indices)) {
         list2env(args_values[named_indices], envir = eval_env)
       }
    }
    
    for (nm in names(f_defs)) {
      if (!nm %in% names(args_values)) {
        def_expr <- f_defs[[nm]]
        if (!is.symbol(def_expr) || as.character(def_expr) != "") {
           if (!nm %in% names(args_exprs)) {
             args_exprs[[nm]] <- def_expr
           }
           val <- tryCatch(eval(def_expr, envir = eval_env), error = function(e) NULL)
           args_values[[nm]] <- val
           assign(nm, val, envir = eval_env)
        }
      }
    }
    
    # 3. Filter Ignored Arguments
    if (!is.null(ignore_args)) {
      args_exprs  <- args_exprs[ !names(args_exprs) %in% ignore_args ]
      args_values <- args_values[ !names(args_values) %in% ignore_args ]
    }
    
    # 4. Sort for Determinism
    if (length(args_exprs) > 0)  args_exprs  <- args_exprs[order(names(args_exprs))]
    if (length(args_values) > 0) args_values <- args_values[order(names(args_values))]
    
    # ---------------------------------------------------------- #
    # B. Compute Hashes
    # ---------------------------------------------------------- #
    
    # Compute Closure Hash
    closure_hash <- .get_recursive_closure_hash(f, algo = algo)
    
    local_path_hasher <- function(p) .get_path_hash(p, file_pattern = file_pattern, algo = algo)
    
    # File Hashing: Recursively scan arguments for paths/connections
    dir_hashes <- character()
    if (length(args_values) > 0) {
      # Use helper to scan deeply into lists and check connections
      paths <- .extract_paths_recursively(args_values)
      # Normalize
      paths <- normalizePath(paths, mustWork = FALSE)
      valid_paths <- unique(paths[file.exists(paths) | dir.exists(paths)])
      
      if (length(valid_paths) > 0) {
        dir_hashes <- vapply(valid_paths, local_path_hasher, character(1L))
        dir_hashes <- dir_hashes[order(names(dir_hashes))]
      }
    }
    
    pkg_versions <- .get_pkg_versions(ast_pkgs)
    
    # Environment Variables
    current_envs <- NULL
    if (!is.null(env_vars)) current_envs <- as.list(Sys.getenv(sort(env_vars), unset = NA))
    
    # Options (Detected via AST)
    current_opts <- NULL
    if (length(ast_opts) > 0) {
      # Capture the current value of options used by the function
      current_opts <- options()[ast_opts]
      # Handle case where option is not set (NULL) to ensure consistency
      current_opts <- current_opts[order(names(current_opts))]
    }
    
    # ---------------------------------------------------------- #
    # C. Build Cache Key
    # ---------------------------------------------------------- #
    hashlist <- list(
      closure     = closure_hash,  # Code logic + Globals
      pkgs        = pkg_versions,  # Package dependencies
      dir_states  = dir_hashes,    # File contents of arguments
      envs        = current_envs,  # Environment variables
      opts        = current_opts,  # R Options detected in code
      args_exprs  = args_exprs,    # Source code of arguments
      args_values = args_values    # Runtime values of arguments
    )
    
    args_hash <- digest::digest(hashlist, algo = algo)
    outfile   <- file.path(cache_dir, paste(fname, args_hash, backend, sep = "."))
    
    # ---------------------------------------------------------- #
    # D. Cache Tree Hooks (External Visualization)
    # ---------------------------------------------------------- #
    node_id <- paste(fname, args_hash, sep = ":")
    if (exists(".cacheTree_register_node", mode = "function")) {
      .cacheTree_register_node(node_id, fname, args_hash, outfile)
      if (exists(".cacheTree_env")) {
        ct_env <- get(".cacheTree_env")
        ct_env$call_stack <- c(ct_env$call_stack, node_id)
        on.exit({
           ct_env <- get(".cacheTree_env")
           ct_env$call_stack <- head(ct_env$call_stack, -1L)
        }, add = TRUE)
      }
      if (length(dir_hashes) > 0 && exists(".cacheTree_add_file", mode = "function")) {
         file_paths <- names(dir_hashes)
         if (!is.null(file_paths)) {
            for (fp in file_paths) try(.cacheTree_add_file(fp, dir_hashes[[fp]]), silent = TRUE)
         }
      }
    }
    
    # ---------------------------------------------------------- #
    # E. LOAD: Check Cache
    # ---------------------------------------------------------- #
    if (.load && file.exists(outfile)) {
      cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
      
      if (!is.null(cached_obj)) {
        if (is.list(cached_obj) && "value" %in% names(cached_obj) && "meta" %in% names(cached_obj)) {
          return(cached_obj$value)
        }
        if (is.list(cached_obj) && "dat" %in% names(cached_obj)) {
          return(cached_obj$dat)
        }
        return(cached_obj)
      }
    }
    
    # ---------------------------------------------------------- #
    # F. MISS: Compute and Save
    # ---------------------------------------------------------- #
    dat <- f(...)
    
    full_meta <- c(hashlist, list(
       fname      = fname,
       args_hash  = args_hash,
       cache_dir  = normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
       cache_file = normalizePath(outfile, winslash = "/", mustWork = FALSE),
       created    = Sys.time()
    ))
    
    save_data <- list(value = dat, meta = full_meta)
    
    do_save <- function() .atomic_save(save_data, outfile, backend)

    if (requireNamespace("filelock", quietly = TRUE)) {
      lockfile <- paste0(outfile, ".lock")
      lock <- tryCatch(filelock::lock(lockfile, timeout = 5000), error = function(e) NULL)
      if (!is.null(lock)) {
        on.exit(filelock::unlock(lock), add = TRUE)
        if (.load && file.exists(outfile)) {
           cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
           if (!is.null(cached_obj)) {
             if (is.list(cached_obj) && "value" %in% names(cached_obj)) return(cached_obj$value)
             if (is.list(cached_obj) && "dat" %in% names(cached_obj)) return(cached_obj$dat)
             return(cached_obj)
           }
        }
        do_save()
      } else { do_save() }
    } else { do_save() }
    
    dat
  }
}

# -------------------------------------------------------------------------
# 4. Persistence Helpers
# -------------------------------------------------------------------------

#' Atomic Save
#' 
#' Saves an object to a temporary file first, then renames it to the target
#' path. This ensures that the cache file is never in a half-written state.
#'
#' @param object The R object to save.
#' @param path Target file path.
#' @param backend "rds" or "qs".
#' @keywords internal
.atomic_save <- function(object, path, backend) {
  tmp_path <- paste0(path, ".tmp.", paste(sample(c(letters, 0:9), 8, replace=TRUE), collapse=""))
  tryCatch({
    if (backend == "qs") {
      if (!requireNamespace("qs", quietly = TRUE)) stop("qs package required")
      qs::qsave(object, tmp_path)
    } else {
      saveRDS(object, tmp_path)
    }
    if (!file.rename(tmp_path, path)) {
      file.copy(tmp_path, path, overwrite = TRUE)
      unlink(tmp_path)
    }
    if (.Platform$OS.type == "unix") try(Sys.chmod(path, mode = "0664", use_umask = FALSE), silent=TRUE)
  }, error = function(e) {
    if (file.exists(tmp_path)) unlink(tmp_path)
    warning(paste("cacheR: Failed to save:", path, "\nError:", e$message))
  })
}

#' Safe Load
#'
#' Wrapper around readRDS/qread to handle backends consistently.
#'
#' @param path Path to cache file.
#' @param backend "rds" or "qs".
#' @keywords internal
.safe_load <- function(path, backend) {
  if (backend == "qs") {
    if (!requireNamespace("qs", quietly = TRUE)) stop("qs package required")
    return(qs::qread(path))
  } else {
    return(readRDS(path))
  }
}

#' Recursive Closure Hasher
#' 
#' Deeply hashes a function object. It recurses into the function's:
#' 1. Body and Formals.
#' 2. Global variables (recursing into lists and detecting file paths).
#' 3. Enclosing environment.
#' 4. Package dependencies.
#' 5. Connections (by hashing the backing file if resolvable).
#'
#' @param obj The object (usually a function) to hash.
#' @param visited Environment to track recursion and avoid infinite loops.
#' @param algo Hash algorithm.
#' @param version_checker Function to retrieve package versions.
#' 
#' @return String hash.
#' @keywords internal
.get_recursive_closure_hash <- function(obj, 
                                        visited = NULL, 
                                        algo = "xxhash64", 
                                        version_checker = utils::packageVersion) { 
  if (is.null(visited)) visited <- new.env(parent = emptyenv())
  
  # Case 1: Function
  if (is.function(obj)) {
    obj_id <- digest::digest(obj, algo = algo)
    if (exists(obj_id, envir = visited)) return(visited[[obj_id]])
    
    # Check if function belongs to a package namespace
    fn_env <- environment(obj)
    if (is.environment(fn_env) && isNamespace(fn_env)) {
      pkg_name <- getNamespaceName(fn_env)
      pkg_ver <- tryCatch(
        as.character(version_checker(pkg_name)), 
        error = function(e) "unknown"
      )
      final_hash <- digest::digest(list(package = pkg_name, version = pkg_ver), algo = algo)
      visited[[obj_id]] <- final_hash
      return(final_hash)
    }

    visited[[obj_id]] <- "RECURSION_CYCLE"
    
    # Hash Body + Formals
    body_hash <- digest::digest(list(formals(obj), body(obj)), algo = algo)
    
    # Find Globals (dependencies)
    globals   <- codetools::findGlobals(obj, merge = TRUE)
    
    # Recursively hash all dependencies
    dep_hashes <- lapply(globals, function(var_name) {
      val <- try(get(var_name, envir = fn_env), silent = TRUE)
      if (inherits(val, "try-error")) return(NULL)
      .get_recursive_closure_hash(val, visited, algo = algo, version_checker = version_checker)
    })
    
    final_hash <- digest::digest(list(body = body_hash, deps = dep_hashes), algo = algo)
    visited[[obj_id]] <- final_hash
    return(final_hash)
  }
  
  # Case 2: Environment
  if (is.environment(obj)) {
    if (isNamespace(obj) || identical(obj, .GlobalEnv) || environmentName(obj) != "") {
      return(digest::digest(environmentName(obj), algo = algo))
    }
    obj_list <- as.list(obj)
    if (length(obj_list) > 0) {
      obj_list <- obj_list[order(names(obj_list))]
      hashes <- lapply(obj_list, function(x) {
        .get_recursive_closure_hash(x, visited, algo = algo, version_checker = version_checker)
      })
      return(digest::digest(hashes, algo = algo))
    }
    return(digest::digest("empty_env", algo = algo))
  }
  
  # Case 3: List (CRITICAL FIX for Global Config List test)
  # Must recurse into lists to detect file paths or connections nested inside.
  if (is.list(obj)) {
     hashes <- lapply(obj, function(x) {
       .get_recursive_closure_hash(x, visited, algo = algo, version_checker = version_checker)
     })
     return(digest::digest(hashes, algo = algo))
  }
  
  # Case 4: Character (Check for files)
  if (is.character(obj)) {
    content_hashes <- vapply(obj, function(x) {
      if (is.na(x) || nchar(x) == 0) return(NA_character_)
      if (file.exists(x) || dir.exists(x)) {
         return(.get_path_hash(x, algo = algo))
      }
      return(NA_character_)
    }, character(1L))
    return(digest::digest(list(val = obj, file_content = content_hashes), algo = algo))
  }
  
  # Case 5: Connection Objects (Base or DBI)
  if (inherits(obj, "connection") || inherits(obj, "DBIConnection")) {
     conn_path <- .get_connection_path(obj)
     if (!is.null(conn_path)) {
        # Hash the backing file of the connection
        return(digest::digest(list(
            conn_class = class(obj), 
            file_hash = .get_path_hash(conn_path, algo = algo)
        ), algo = algo))
     }
  }
  
  # Default: Standard object hash
  return(digest::digest(obj, algo = algo))
}