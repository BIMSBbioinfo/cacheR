#' Create a disk-backed caching decorator for a function
#'
#' @param cache_dir    Directory where cache files will be stored.
#' @param backend      Storage backend: "rds" (default) or "qs".
#' @param file_args    Optional character vector of argument names to strictly treat as paths.
#' @param ignore_args  Character vector of argument names to exclude from the hash.
#' @param file_pattern Regex pattern to filter files when hashing directories.
#' @param env_vars     Character vector of environment variable names to track.
#' @param algo         Hashing algorithm to use (default "xxhash64").
#'
#' @return A decorator object usable via `%@%`.
#' @export
#' File-based Cache Decorator
#'
#' @param cache_dir Directory to store cache files. Defaults to a package-wide default if NULL.
#' @param backend Serialization backend ("rds" or "qs").
#' @param file_args Optional list of arguments to scan for file paths (for automatic invalidation).
#' @param ignore_args Optional list of arguments to exclude from the hash generation.
#' @param file_pattern Regex pattern for file matching when hashing directory contents.
#' @param env_vars Character vector of environment variable names to track.
#' @param algo Hashing algorithm to use (passed to digest::digest).
#'
#' @return A decorator function.
#' @export
#' File-based Cache Decorator
#'
#' @param cache_dir Directory to store cache files.
#' @param backend Serialization backend ("rds" or "qs").
#' @param file_args Optional list of arguments to scan for file paths (for automatic invalidation).
#' @param ignore_args Optional list of arguments to exclude from the hash generation.
#' @param file_pattern Regex pattern for file matching when hashing directory contents.
#' @param env_vars Character vector of environment variable names to track.
#' @param algo Hashing algorithm to use (passed to digest::digest).
#'
#' @return A decorator function.
#' @export
#' File-based Cache Decorator
#'
#' @param cache_dir Directory to store cache files.
#' @param backend Serialization backend ("rds" or "qs").
#' @param file_args Optional list of arguments to scan for file paths.
#' @param ignore_args Optional list of arguments to exclude from the hash.
#' @param file_pattern Regex pattern for file matching when hashing directory contents.
#' @param env_vars Character vector of environment variable names to track.
#' @param algo Hashing algorithm to use (passed to digest::digest).
#'
#' @return A decorator function.
#' @export
#' @param cache_dir Directory to store files
#' @param ignore_args Character vector of argument names to exclude from the hash
#' @export
cacheFile <- function(cache_dir, ignore_args = NULL) {
  
  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  function(f) {
    # Get function name for the graph
    fname <- deparse(substitute(f))
    if (length(fname) > 1) fname <- "anonymous_function"
    
    wrapper <- function(...) {
      # 1. Capture and process arguments
      args_list <- as.list(match.call())[-1] # remove function name
      
      # Fill in defaults for missing arguments so hash is stable
      formals_list <- formals(f)
      # Match args to formals
      matched_args <- match.call(definition = f, expand.dots = TRUE)[-1]
      
      # 2. Hash Calculation (excluding ignore_args)
      hash_input <- as.list(matched_args)
      if (!is.null(ignore_args)) {
        hash_input <- hash_input[setdiff(names(hash_input), ignore_args)]
      }
      
      # Create a deterministic hash of input + function body
      args_hash <- digest::digest(list(body(f), hash_input), algo = "md5")
      outfile   <- file.path(cache_dir, paste0(fname, "_", args_hash, ".rds"))

      # -------------------------------------------------------------------
      # [TREE INTEGRATION] Register Node & Manage Stack
      # -------------------------------------------------------------------
      # A unique ID for this specific execution node
      node_id <- args_hash 
      
      # Push to stack
      .cacheTree_env$call_stack <- c(.cacheTree_env$call_stack, node_id)
      
      # Register in graph
      .cacheTree_register_node(node_id, fname, args_hash, outfile)
      
      # Pop from stack when function exits (success or failure)
      on.exit({
        stack <- .cacheTree_env$call_stack
        if (length(stack) > 0) {
          .cacheTree_env$call_stack <- stack[-length(stack)]
        }
      }, add = TRUE)
      # -------------------------------------------------------------------

      # 3. Check Cache
      if (file.exists(outfile)) {
        message(sprintf("Cache HIT: %s", basename(outfile)))
        
        # -----------------------------------------------------------------
        # [TOUCH MECHANISM] Update mtime to now
        # -----------------------------------------------------------------
        # This prevents cachePrune() from deleting frequently read files
        Sys.setFileTime(outfile, Sys.time()) 
        # -----------------------------------------------------------------
        
        return(readRDS(outfile))
      }

      # 4. Cache MISS: Run and Save
      message(sprintf("Cache MISS: Running %s...", fname))
      
      # Execute the actual function
      res <- f(...)
      
      saveRDS(res, outfile)
      return(res)
    }
    
    return(wrapper)
  }
}

#' @importFrom digest digest
#' @importFrom utils packageVersion
#' @importFrom codetools findGlobals
#' Recursive Function Hasher
#'
#' @param obj The object (usually a function) to hash.
#' @param visited An environment used internally to detect circular dependencies.
#' @param algo The hashing algorithm to use (passed to digest::digest).
#'
#' @return A character string (hash).
#' Recursive Function Hasher
#'
#' @param obj The object (usually a function) to hash.
#' @param visited An environment used internally to detect circular dependencies.
#' @param algo The hashing algorithm to use (passed to digest::digest).
#'
#' @return A character string (hash).
.get_recursive_closure_hash <- function(obj, visited = NULL, algo = "xxhash64") {
  
  # 1. Initialize cycle detection on first call
  if (is.null(visited)) {
    visited <- new.env(parent = emptyenv())
  }
  
  # 2. Handle non-functions (data objects)
  if (!is.function(obj)) {
    return(digest::digest(obj, algo = algo))
  }
  
  # 3. Handle Primitives (e.g., sum, sin, c)
  if (is.primitive(obj)) {
    return(digest::digest(obj, algo = algo))
  }
  
  # 4. Cycle Detection
  obj_id <- digest::digest(obj, algo = algo) 
  
  if (exists(obj_id, envir = visited)) {
    return(visited[[obj_id]]) # Return cached partial hash
  }
  
  # Mark as visited
  visited[[obj_id]] <- "RECURSION_CYCLE"
  
  # 5. Handle Package Functions
  func_env <- environment(obj)
  is_ns <- !is.null(func_env) && isNamespace(func_env)
  
  if (is_ns) {
    pkg_name <- getNamespaceName(func_env)
    
    # We generally don't recurse into base packages or installed libraries.
    # We just hash the version number so that if the package updates, the cache invalidates.
    if (pkg_name != "base") {
      tryCatch({
        ver <- utils::packageVersion(pkg_name)
        return(digest::digest(list(package = pkg_name, version = ver), algo = algo))
      }, error = function(e) {
        # If version lookup fails, fall through to hashing the body
      })
    }
  }
  
  # 6. Extract Hashable Components
  
  # A. Body and Formals
  body_hash <- digest::digest(list(formals(obj), body(obj)), algo = algo)
  
  # B. Dependencies (Globals)
  # FIX: Use merge = TRUE to capture both variables AND functions called.
  globals <- codetools::findGlobals(obj, merge = TRUE)
  
  dep_hashes <- lapply(globals, function(var_name) {
    # Attempt to retrieve the variable from the function's environment
    val <- try(get(var_name, envir = func_env), silent = TRUE)
    
    if (inherits(val, "try-error")) {
      return(NULL) 
    }
    
    # RECURSE: Pass the algo down
    .get_recursive_closure_hash(val, visited, algo = algo)
  })
  
  # 7. Final Hash
  final_hash <- digest::digest(list(body = body_hash, deps = dep_hashes), algo = algo)
  
  visited[[obj_id]] <- final_hash
  
  return(final_hash)
}

## -------------------------------------------------------------------------
## Helper: Find Path Dependencies (.find_path_specs) (UNCHANGED)
## -------------------------------------------------------------------------
.find_path_specs <- function(expr) {
  literal_dirs <- character()
  symbol_dirs  <- character()
  target_funs  <- c("list.files", "dir", "list.dirs")

  recurse <- function(sub_node, collect = FALSE) {
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
      if (is.symbol(head)) {
        fname <- as.character(head)
      } else if (is.call(head)) {
        hp <- as.character(head)
        if (length(hp) >= 3 && hp[1] %in% c("::", ":::")) fname <- hp[3]
      }
      is_target <- !is.null(fname) && fname %in% target_funs
      if (is_target && !collect) {
        args      <- as.list(sub_node)[-1L]
        arg_names <- names(args)
        target    <- if ("path" %in% arg_names) args[["path"]] else if (length(args) >= 1) args[[1L]] else NULL
        if (!is.null(target)) recurse(target, collect = TRUE)
        if (length(args)) {
          for (j in seq_along(args)) {
            if (!is.null(target) && identical(args[[j]], target)) next
            recurse(args[[j]], collect = FALSE)
          }
        }
      } else {
        for (j in seq_along(sub_node)) recurse(sub_node[[j]], collect = collect)
      }
    } else if (is.pairlist(sub_node)) {
      for (j in seq_along(sub_node)) recurse(sub_node[[j]], collect = collect)
    }
  }
  tryCatch(recurse(expr), error = function(e) warning(".find_path_specs failed: ", conditionMessage(e)))
  list(literals = unique(literal_dirs), symbols = unique(symbol_dirs))
}