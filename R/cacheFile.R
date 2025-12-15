# -------------------------------------------------------------------------
# 1. Global State (Package Level)
# -------------------------------------------------------------------------
.file_state_cache <- new.env(parent = emptyenv())

# -------------------------------------------------------------------------
# 2. Helper Functions
# -------------------------------------------------------------------------

#' Probabilistic File Hashing
#' Reads header, footer, and random blocks to hash large files efficiently.
.probabilistic_file_hash <- function(path, block_size = 64 * 1024, n_blocks = 5, algo = "xxhash64") {
  if (!file.exists(path)) return(NA_character_)
  
  info <- file.info(path)
  size <- info$size
  if (is.na(size)) return(NA_character_)
  
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  
  blocks <- list()
  blocks[[1]] <- readBin(con, "raw", block_size)
  
  if (size > block_size) {
    max_offset <- max(size - block_size, 1)
    
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    }
    
    seed_str <- paste0(path, size)
    seed_int <- sum(as.integer(charToRaw(digest::digest(seed_str, algo = "crc32"))))
    set.seed(seed_int)
    
    for (i in seq_len(n_blocks)) {
      offset <- sample.int(max_offset, 1)
      seek(con, offset, "start")
      blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
    }
    
    seek(con, max(size - block_size, 0), "start")
    blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  }
  
  bytes <- do.call(c, blocks)
  digest::digest(bytes, algo = algo)
}

.fast_file_hash <- function(path, algo = "xxhash64") {
  info <- file.info(path)
  if (is.na(info$size)) return(NA_character_)
  fp <- paste(info$size, unclass(info$mtime), sep = "|")
  prev <- .file_state_cache[[path]]
  if (!is.null(prev) && identical(prev$fp, fp)) return(prev$hash)
  h <- .probabilistic_file_hash(path, algo = algo)
  .file_state_cache[[path]] <- list(fp = fp, hash = h)
  h
}

.get_path_hash <- function(path, file_pattern = NULL, algo = "xxhash64") {
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE, pattern = file_pattern)
    if (length(files) == 0) return("empty_dir")
    files <- sort(files)
    hashes <- vapply(files, .fast_file_hash, character(1L), algo = algo)
    return(digest::digest(hashes, algo = algo))
  } 
  if (file.exists(path)) return(.fast_file_hash(path, algo = algo))
  return(NULL)
}

# -------------------------------------------------------------------------
# 3. Main Decorator
# -------------------------------------------------------------------------

#' Create a robust, disk-backed caching decorator
#' @export
#' Create a robust, disk-backed caching decorator
#' 
#' @param cache_dir Directory to store cache files.
#' @param backend Serialization backend ("rds" or "qs").
#' @param ignore_args Character vector of argument names to exclude from the hash.
#' @param file_pattern Regex pattern for file dependencies.
#' @param env_vars Character vector of environment variables to include in the hash.
#' @param algo Hashing algorithm to use (default "xxhash64").
#' @export
cacheFile <- function(cache_dir    = NULL,
                      backend      = getOption("cacheR.backend", "rds"),
                      ignore_args  = NULL,
                      file_pattern = NULL,
                      env_vars     = NULL,
                      algo         = "xxhash64") decorator %@% function(f) {
  
  if (is.null(cache_dir)) cache_dir <- cacheR_default_dir()
  if (!dir.exists(cache_dir)) {
    tryCatch(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE),
             error = function(e) warning("cacheR: Could not create cache directory."))
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  backend   <- match.arg(backend, c("rds", "qs"))
  
  function(..., .load = TRUE) {
    invoke_env <- parent.frame()
    full_call  <- match.call(expand.dots = TRUE)
    
    fname_raw <- full_call[[1]]
    fname     <- "anon"
    if (is.symbol(fname_raw)) fname <- as.character(fname_raw)
    else if (is.character(fname_raw)) fname <- fname_raw
    fname <- gsub("[^a-zA-Z0-9_]", "_", fname)
    if (nchar(fname) > 50) fname <- substring(fname, 1, 50)
    
    # ---------------------------------------------------------- #
    # 1. Parse and Standardize Arguments
    # ---------------------------------------------------------- #
    call_matched  <- match.call(definition = f, expand.dots = TRUE)
    args_supplied <- as.list(call_matched)[-1]
    f_formals     <- formals(f)
    f_formals     <- f_formals[names(f_formals) != "..."]
    
    args_for_hash <- f_formals
    for (nm in names(args_supplied)) args_for_hash[[nm]] <- args_supplied[[nm]]
    
    if ("..." %in% names(f_formals) || "..." %in% names(args_supplied)) {
       extra_args <- args_supplied[ !names(args_supplied) %in% names(f_formals) ]
       args_for_hash <- c(args_for_hash, extra_args)
    }
    if (!is.null(ignore_args)) args_for_hash <- args_for_hash[ !names(args_for_hash) %in% ignore_args ]
    if (length(args_for_hash) > 0) args_for_hash <- args_for_hash[order(names(args_for_hash))]
    
    # ---------------------------------------------------------- #
    # 2. Compute Hashes (Args, Files, Closure, Env)
    # ---------------------------------------------------------- #
    local_path_hasher <- function(p) .get_path_hash(p, file_pattern = file_pattern, algo = algo)
    evaluated_args <- lapply(args_for_hash, function(expr) {
      tryCatch(eval(expr, envir = invoke_env), error = function(e) NULL)
    })
    
    dir_hashes <- character()
    if (length(evaluated_args)) {
      hash_list <- list()
      for (val in evaluated_args) {
        if (is.null(val) || !is.character(val) || length(val) == 0) next
        paths <- normalizePath(val, mustWork = FALSE)
        valid_paths <- paths[file.exists(paths) | dir.exists(paths)]
        if (length(valid_paths) == 0) next
        res <- vapply(unique(valid_paths), local_path_hasher, character(1L))
        hash_list <- c(hash_list, list(res))
      }
      if (length(hash_list) > 0) dir_hashes <- unlist(hash_list)
    }
    
    current_envs <- NULL
    if (!is.null(env_vars)) current_envs <- as.list(Sys.getenv(sort(env_vars), unset = NA))
    deep_hash <- .get_recursive_closure_hash(f, algo = algo)
    
    hashlist <- list(
      args       = args_for_hash,
      closure    = deep_hash,
      dir_states = dir_hashes,
      envs       = current_envs
    )
    
    args_hash <- digest::digest(hashlist, algo = algo)
    outfile   <- file.path(cache_dir, paste(fname, args_hash, backend, sep = "."))
    
    # ---------------------------------------------------------- #
    # 3. Cache Tree Hooks (Optional)
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
    # 4. LOAD: Check Cache
    # ---------------------------------------------------------- #
    if (.load && file.exists(outfile)) {
      cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
      
      if (!is.null(cached_obj)) {
        # New Format: list(value = ..., meta = ...)
        if (is.list(cached_obj) && "value" %in% names(cached_obj) && "meta" %in% names(cached_obj)) {
          return(cached_obj$value)
        }
        # Legacy Format: list(dat = ..., meta = ...)
        if (is.list(cached_obj) && "dat" %in% names(cached_obj)) {
          return(cached_obj$dat)
        }
        # Fallback: Raw object
        return(cached_obj)
      }
    }
    
    # ---------------------------------------------------------- #
    # 5. MISS: Compute and Save
    # ---------------------------------------------------------- #
    dat <- f(...)
    
    # Construct Full Metadata (Satisfies cacheInfo tests)
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
        # Double-check cache after acquiring lock (Race condition prevention)
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

.safe_load <- function(path, backend) {
  if (backend == "qs") {
    if (!requireNamespace("qs", quietly = TRUE)) stop("qs package required")
    return(qs::qread(path)$dat)
  } else {
    return(readRDS(path)$dat)
  }
}

#' Recursive Closure Hasher
#' Tracks Functions, Environments, and Global Variables.
#' If a variable contains a file path, it hashes the file content!
.get_recursive_closure_hash <- function(obj, 
                                        visited = NULL, 
                                        algo = "xxhash64", 
                                        version_checker = utils::packageVersion) { # [1] Inject Dependency
  
  if (is.null(visited)) visited <- new.env(parent = emptyenv())
  
  # 1. Functions
  if (is.function(obj)) {
    obj_id <- digest::digest(obj, algo = algo)
    if (exists(obj_id, envir = visited)) return(visited[[obj_id]])
    
    # [2] New Logic: Check if function belongs to a package (Namespace)
    # If so, hash the Package Name + Version, and STOP recursing.
    fn_env <- environment(obj)
    if (is.environment(fn_env) && isNamespace(fn_env)) {
      pkg_name <- getNamespaceName(fn_env)
      
      # Use the INJECTED version checker
      pkg_ver <- tryCatch(
        as.character(version_checker(pkg_name)), 
        error = function(e) "unknown"
      )
      
      final_hash <- digest::digest(list(package = pkg_name, version = pkg_ver), algo = algo)
      visited[[obj_id]] <- final_hash
      return(final_hash)
    }

    visited[[obj_id]] <- "RECURSION_CYCLE"
    
    body_hash <- digest::digest(list(formals(obj), body(obj)), algo = algo)
    globals   <- codetools::findGlobals(obj, merge = TRUE)
    # func_env is already defined above as fn_env
    
    dep_hashes <- lapply(globals, function(var_name) {
      val <- try(get(var_name, envir = fn_env), silent = TRUE)
      if (inherits(val, "try-error")) return(NULL)
      
      # [3] Pass version_checker down recursively
      .get_recursive_closure_hash(val, visited, algo = algo, version_checker = version_checker)
    })
    
    final_hash <- digest::digest(list(body = body_hash, deps = dep_hashes), algo = algo)
    visited[[obj_id]] <- final_hash
    return(final_hash)
  }
  
  # 2. Environments (Unpack and Recurse)
  # We skip Namespaces (packages) and GlobalEnv to avoid performance issues.
  if (is.environment(obj)) {
    if (isNamespace(obj) || identical(obj, .GlobalEnv) || environmentName(obj) != "") {
      return(digest::digest(environmentName(obj), algo = algo))
    }
    # Convert User Environment to List
    obj_list <- as.list(obj)
    if (length(obj_list) > 0) {
      obj_list <- obj_list[order(names(obj_list))]
      
      # [4] Pass version_checker down recursively inside environments
      hashes <- lapply(obj_list, function(x) {
        .get_recursive_closure_hash(x, visited, algo = algo, version_checker = version_checker)
      })
      return(digest::digest(hashes, algo = algo))
    }
    return(digest::digest("empty_env", algo = algo))
  }
  
  # 3. Strings: File Detection (The "Scan Everything" logic)
  if (is.character(obj)) {
    content_hashes <- vapply(obj, function(x) {
      if (is.na(x) || nchar(x) == 0) return(NA_character_)
      # Safe check if it's a file path
      if (file.exists(x) || dir.exists(x)) {
         return(.get_path_hash(x, algo = algo))
      }
      return(NA_character_)
    }, character(1L))
    
    # Hash the string value AND the file content it might point to
    return(digest::digest(list(val = obj, file_content = content_hashes), algo = algo))
  }
  
  # 4. Default
  return(digest::digest(obj, algo = algo))
}