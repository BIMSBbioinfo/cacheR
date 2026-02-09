# =========================================================================
# cacheR: Robust Disk-Backed Memoization for R
# =========================================================================

# -------------------------------------------------------------------------
# 1. Global State
# -------------------------------------------------------------------------

#' File State Cache
#' @keywords internal
.file_state_cache <- new.env(parent = emptyenv())

#' Execution Graph Cache
#' @keywords internal
.graph_cache <- new.env(parent = emptyenv())
.graph_cache$nodes <- new.env(parent = emptyenv())
.graph_cache$edges <- new.env(parent = emptyenv())
.graph_cache$call_stack <- character()

#' Reset the Graph Tracker
#' @export
cacheR_reset_graph <- function() {
  .graph_cache$nodes <- new.env(parent = emptyenv())
  .graph_cache$edges <- new.env(parent = emptyenv())
  .graph_cache$call_stack <- character()
}

#' Alias for reset (for test compatibility)
#' @export
cacheTree_reset <- cacheR_reset_graph

# -------------------------------------------------------------------------
# 2. Graph & Tracking Helpers
# -------------------------------------------------------------------------

#' Get all nodes
#' @export
cacheTree_nodes <- function() {
  as.list(.graph_cache$nodes)
}

#' Append graph changes to disk (Persistence Layer)
#' @keywords internal
.append_graph_to_disk <- function(cache_dir, new_node = NULL, new_edge = NULL) {
  if (is.null(cache_dir)) return()
  
  graph_file <- file.path(cache_dir, "graph.rds")
  lock_file  <- paste0(graph_file, ".lock")
  
  # Try to acquire lock
  if (requireNamespace("filelock", quietly = TRUE)) {
    lock <- tryCatch(filelock::lock(lock_file, timeout = 5000), error = function(e) NULL)
    if (!is.null(lock)) on.exit(filelock::unlock(lock), add = TRUE)
  }
  
  # Load existing
  current_graph <- list(nodes = list(), edges = list())
  if (file.exists(graph_file)) {
    try({ current_graph <- readRDS(graph_file) }, silent = TRUE)
  }
  
  changed <- FALSE
  
  # Append Node
  if (!is.null(new_node)) {
    # Check by ID to prevent duplicates
    existing_ids <- vapply(current_graph$nodes, function(n) n$id, character(1))
    if (!new_node$id %in% existing_ids) {
      current_graph$nodes[[length(current_graph$nodes) + 1]] <- new_node
      changed <- TRUE
    }
  }
  
  # Append Edge
  if (!is.null(new_edge)) {
    edge_sig <- function(e) paste(e$from, e$to, sep="|")
    existing_sigs <- vapply(current_graph$edges, edge_sig, character(1))
    new_sig <- edge_sig(new_edge)
    
    if (!new_sig %in% existing_sigs) {
      current_graph$edges[[length(current_graph$edges) + 1]] <- new_edge
      changed <- TRUE
    }
  }
  
  if (changed) saveRDS(current_graph, graph_file)
}

#' Register a Node
#' @keywords internal
.register_node <- function(id, type, label = id, code = NULL, cache_dir = NULL) {
  # 1. Update In-Memory
  if (!exists(id, envir = .graph_cache$nodes)) {
    node <- list(
      id = id, type = type, label = label, code = code,
      files = character(), file_hashes = list(),
      children = character(), parents = character(),
      time = Sys.time()
    )
    assign(id, node, envir = .graph_cache$nodes)
    
    # 2. Update Disk
    .append_graph_to_disk(cache_dir, new_node = node)
  }
}

#' Register an Edge
#' @keywords internal
.register_edge <- function(from, to, cache_dir = NULL) {
  edge_id <- paste(from, to, sep = "->")
  
  if (!exists(edge_id, envir = .graph_cache$edges)) {
    edge <- list(from = from, to = to)
    assign(edge_id, edge, envir = .graph_cache$edges)
    
    # Update In-Memory Links
    if (exists(from, envir = .graph_cache$nodes)) {
      n <- get(from, envir = .graph_cache$nodes)
      n$children <- unique(c(n$children, to))
      assign(from, n, envir = .graph_cache$nodes)
    }
    if (exists(to, envir = .graph_cache$nodes)) {
      n <- get(to, envir = .graph_cache$nodes)
      n$parents <- unique(c(n$parents, from))
      assign(to, n, envir = .graph_cache$nodes)
    }
    
    # Update Disk
    .append_graph_to_disk(cache_dir, new_edge = edge)
  }
}

#' Find nodes associated with a specific file path
#' @export
cacheTree_for_file <- function(path) {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  all_nodes <- cacheTree_nodes()
  Filter(function(x) path %in% x$files, all_nodes)
}

#' Load graph from disk into memory
#' @export
cacheTree_sync <- function(cache_dir) {
  graph_file <- file.path(cache_dir, "graph.rds")
  if (file.exists(graph_file)) {
    data <- tryCatch(readRDS(graph_file), error = function(e) NULL)
    if (!is.null(data)) {
      # Load Nodes
      for (n in data$nodes) {
        if (!exists(n$id, envir = .graph_cache$nodes)) {
          assign(n$id, n, envir = .graph_cache$nodes)
        }
      }
      # Load Edges
      for (e in data$edges) {
        edge_id <- paste(e$from, e$to, sep = "->")
        if (!exists(edge_id, envir = .graph_cache$edges)) {
          assign(edge_id, e, envir = .graph_cache$edges)
        }
      }
    }
  }
}

#' Explicitly Track a File Dependency
#' @export
track_file <- function(path, cache_dir = NULL) {
  path_norm <- normalizePath(path, mustWork = FALSE, winslash = "/")
  
  if (dir.exists(path_norm)) {
    hash <- .get_path_hash(path_norm)
  } else {
    hash <- .fast_file_hash(path_norm)
  }
  
  # Register the file as a node
  .register_node(id = path_norm, type = "file", label = basename(path_norm), cache_dir = cache_dir)
  
  if (length(.graph_cache$call_stack) > 0) {
    active_node_id <- tail(.graph_cache$call_stack, 1)
    
    # Register Edge: Function -> File
    .register_edge(from = active_node_id, to = path_norm, cache_dir = cache_dir)
    
    # Update Function Node Metadata
    if (exists(active_node_id, envir = .graph_cache$nodes)) {
      node <- get(active_node_id, envir = .graph_cache$nodes)
      if (!path_norm %in% node$files) {
        node$files <- c(node$files, path_norm)
      }
      node$file_hashes[[path_norm]] <- hash
      assign(active_node_id, node, envir = .graph_cache$nodes)
      
      # We don't save metadata updates to disk immediately to save IO, 
      # but ideally, this update should be synced. 
      # For now, we rely on the node creation snapshot.
    }
  }
  return(path)
}

# -------------------------------------------------------------------------
# 3. Hashing Helpers
# -------------------------------------------------------------------------

#' Probabilistic File Hashing
#' @keywords internal
#' @export
.probabilistic_file_hash <- function(path, block_size = 64 * 1024, n_blocks = 5,
                                      algo = "xxhash64", full_hash_limit = 5 * 1024 * 1024) {
  path <- normalizePath(path, mustWork = FALSE)
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)

  info <- file.info(path)
  size <- info$size
  if (is.na(size)) return(NA_character_)

  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(NA_character_)
  on.exit(close(con), add = TRUE)

  # Full hash for small-to-medium files
  if (size <= full_hash_limit) {
    bytes <- readBin(con, "raw", size)
    return(digest::digest(bytes, algo = algo))
  }

  # Probabilistic hash for large files
  blocks <- list()
  blocks[[1]] <- readBin(con, "raw", block_size)

  max_offset <- max(size - block_size, 1)
  seed_int <- strtoi(substring(digest::digest(paste0(path, size), algo = "crc32"), 1, 7), base = 16L)
  for (i in seq_len(n_blocks)) {
    offset <- (seed_int * i) %% max_offset + 1L
    seek(con, offset, "start")
    blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  }
  seek(con, max(size - block_size, 0), "start")
  blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)

  bytes <- do.call(c, blocks)
  digest::digest(bytes, algo = algo)
}

#' Fast File Hash
#' @keywords internal
#' @export
.fast_file_hash <- function(path, algo = "xxhash64") {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  if (dir.exists(path)) return(digest::digest("directory_placeholder", algo=algo))
  
  info <- file.info(path)
  if (is.na(info$size)) return(NA_character_)
  
  fp <- paste(info$size, unclass(info$mtime), sep = "|")
  prev <- tryCatch(get(path, envir = .file_state_cache), error = function(e) NULL)
  
  if (!is.null(prev) && identical(prev$fp, fp)) return(prev$hash)
  
  h <- .probabilistic_file_hash(path, algo = algo)
  assign(path, list(fp = fp, hash = h), envir = .file_state_cache)
  h
}

#' Get Connection Path
#' @keywords internal
.get_connection_path <- function(x) {
  if (inherits(x, "connection")) {
    try_desc <- try(summary(x)$description, silent = TRUE)
    if (!inherits(try_desc, "try-error") && is.character(try_desc) && file.exists(try_desc)) return(try_desc)
  }
  if (inherits(x, "SQLiteConnection")) {
     if (requireNamespace("DBI", quietly = TRUE)) {
       info <- try(DBI::dbGetInfo(x), silent = TRUE)
       if (!inherits(info, "try-error") && is.list(info) && "dbname" %in% names(info) && file.exists(info$dbname)) return(info$dbname)
     }
     if (isS4(x) && "dbname" %in% slotNames(x) && file.exists(slot(x, "dbname"))) return(slot(x, "dbname"))
  }
  return(NULL)
}

#' Extract Paths Recursively
#' @keywords internal
.extract_paths_recursively <- function(x) {
  paths <- character()
  if (is.character(x)) {
    valid <- x[file.exists(x) | dir.exists(x)]
    if (length(valid) > 0) paths <- c(paths, valid)
  } else if (is.list(x)) {
    for (el in x) paths <- c(paths, .extract_paths_recursively(el))
  } else if (is.environment(x) && !isNamespace(x) && !identical(x, .GlobalEnv)) {
    paths <- c(paths, .extract_paths_recursively(as.list(x)))
  } else if (inherits(x, "connection") || inherits(x, "DBIConnection")) {
    p <- .get_connection_path(x)
    if (!is.null(p)) paths <- c(paths, p)
  } else if (isS4(x)) {
    for (sn in slotNames(x)) {
      paths <- c(paths, .extract_paths_recursively(slot(x, sn)))
    }
  }
  # Check custom attributes for file paths (applies to any object type)
  attrs <- attributes(x)
  if (!is.null(attrs)) {
    std_attrs <- c("names", "class", "dim", "dimnames", "row.names",
                   "levels", "tsp", "comment")
    custom_attrs <- attrs[!names(attrs) %in% std_attrs]
    for (a in custom_attrs) {
      paths <- c(paths, .extract_paths_recursively(a))
    }
  }
  return(paths)
}

#' Substitute Paths with Hashes
#' @keywords internal
.replace_paths_with_hashes <- function(x, file_pattern = NULL, algo = "xxhash64") {
  if (is.character(x)) {
    if (length(x) == 1 && !is.na(x) && (file.exists(x) || dir.exists(x))) {
       if (dir.exists(x)) {
         files <- list.files(x, pattern = file_pattern, recursive = TRUE, full.names = FALSE)
         if (length(files) == 0) return(digest::digest("empty_dir", algo = algo))
         files <- sort(files)
         full_paths <- file.path(x, files)
         hashes <- vapply(full_paths, .fast_file_hash, character(1L), algo = algo)
         return(digest::digest(list(rel=files, dat=hashes), algo = algo))
       } else {
         return(.fast_file_hash(x, algo = algo))
       }
    }
    if (length(x) > 1) {
      return(vapply(x, function(el) {
        if (!is.na(el) && (file.exists(el) || dir.exists(el))) {
          if (dir.exists(el)) {
            .replace_paths_with_hashes(el, file_pattern, algo)
          } else {
            .fast_file_hash(el, algo)
          }
        } else {
          el
        }
      }, character(1)))
    }
    return(x)
  } else if (is.list(x)) {
    return(lapply(x, .replace_paths_with_hashes, file_pattern=file_pattern, algo=algo))
  } else if (is.environment(x) && !isNamespace(x) && !identical(x, .GlobalEnv)) {
    return(lapply(as.list(x), .replace_paths_with_hashes, file_pattern=file_pattern, algo=algo))
  } else if (inherits(x, "connection") || inherits(x, "DBIConnection")) {
     p <- .get_connection_path(x)
     if (!is.null(p)) return(.fast_file_hash(p, algo = algo))
  } else if (isS4(x)) {
    slot_hashes <- list()
    for (sn in slotNames(x)) {
      slot_hashes[[sn]] <- .replace_paths_with_hashes(slot(x, sn), file_pattern, algo)
    }
    return(digest::digest(slot_hashes, algo = algo))
  }
  # Check custom attributes for file paths
  attrs <- attributes(x)
  if (!is.null(attrs)) {
    std_attrs <- c("names", "class", "dim", "dimnames", "row.names",
                   "levels", "tsp", "comment")
    custom_attrs <- attrs[!names(attrs) %in% std_attrs]
    if (length(custom_attrs) > 0) {
      hashed_attrs <- lapply(custom_attrs, .replace_paths_with_hashes,
                             file_pattern = file_pattern, algo = algo)
      if (!identical(hashed_attrs, custom_attrs)) {
        return(digest::digest(list(value = x, hashed_attrs = hashed_attrs), algo = algo))
      }
    }
  }
  return(x)
}

#' Scan AST for Dependencies
#' @keywords internal
.scan_ast_deps <- function(expr) {
  pkgs <- character(); opts <- character()
  walker <- function(e) {
    if (is.call(e)) {
      fn_name <- tryCatch(as.character(e[[1]]), error = function(z) "")
      if (length(fn_name) == 1 && fn_name %in% c("library", "require", "requireNamespace")) {
        if (length(e) >= 2) {
          arg <- e[[2]]
          if (is.character(arg)) pkgs <<- c(pkgs, arg)
          else if (is.symbol(arg)) pkgs <<- c(pkgs, as.character(arg))
        }
      }
      if (length(fn_name) == 1 && fn_name == "getOption") {
        if (length(e) >= 2 && is.character(e[[2]])) opts <<- c(opts, e[[2]])
      }
      lapply(e, walker)
    }
  }
  walker(expr)
  list(pkgs = unique(pkgs), opts = unique(opts))
}

#' Get Package Versions
#' @keywords internal
.get_pkg_versions <- function(pkgs) {
  if (length(pkgs) == 0) return(NULL)
  vapply(pkgs, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NA"), character(1))
}

#' Robust Path Hashing
#' @keywords internal
.get_path_hash <- function(path, file_pattern = NULL, algo = "xxhash64") {
  path <- normalizePath(path, mustWork = FALSE)
  if (dir.exists(path)) {
    files <- list.files(path, pattern = file_pattern, recursive = TRUE, full.names = FALSE)
    if (length(files) == 0) return(digest::digest("empty_dir", algo = algo))
    files <- sort(files)
    full_paths <- file.path(path, files)
    hashes <- vapply(full_paths, .fast_file_hash, character(1L), algo = algo)
    dir_data <- list(relative_paths = files, content_hashes = hashes)
    return(digest::digest(dir_data, algo = algo))
  } 
  if (file.exists(path)) return(.fast_file_hash(path, algo = algo))
  return(NULL)
}

#' Helper: Standardize arguments to ignore variable names and fill defaults
#' @keywords internal
.get_canonical_values <- function(fun, call_obj, env) {
  # 1. Standardize the call (matches args to names, fixes order)
  matched_call <- match.call(definition = fun, call = call_obj, expand.dots = FALSE)

  # 2. Extract explicit arguments passed by user
  args_list <- as.list(matched_call)[-1]

  # 3. Identify missing (default) arguments
  defaults <- formals(fun)
  missing_args <- setdiff(names(defaults), names(args_list))

  # 4. Handle '...' specifically
  if ("..." %in% names(args_list)) {
    args_list[["..."]] <- NULL
  }

  # 5. Evaluate user-supplied args in the caller's env
  eval_values <- lapply(args_list, function(expr) {
    tryCatch(eval(expr, envir = env), error = function(e) expr)
  })

  # 6. Build a local env with evaluated args for default evaluation
  #    Parent is the function's own environment (closure) so defaults
  #    can reference both other args and captured variables.
  default_env <- list2env(eval_values, parent = environment(fun))

  # 7. Evaluate missing defaults in the local env
  for (arg in missing_args) {
    if (arg != "...") {
      eval_values[[arg]] <- tryCatch(
        eval(defaults[[arg]], envir = default_env),
        error = function(e) defaults[[arg]]
      )
    }
  }

  # 8. Sort by name (guard against zero-arg functions)
  if (length(eval_values) > 0) {
    eval_values <- eval_values[order(names(eval_values))]
  }
  return(eval_values)
}

#' Helper: Hash ONLY the globals used by the function (Scope Isolation)
#' @keywords internal
.get_scoped_env_hash <- function(fun) {
  if (!requireNamespace("codetools", quietly = TRUE)) {
    # warning("codetools not installed; falling back to hashing full environment")
    return(digest::digest(environment(fun)))
  }
  
  used_vars <- codetools::findGlobals(fun, merge = FALSE)$variables
  env <- environment(fun)
  captured_globals <- list()
  
  for (var in used_vars) {
    if (exists(var, envir = env, inherits = TRUE)) {
      val <- get(var, envir = env, inherits = TRUE)
      if (is.function(val) && !is.null(environment(val))) {
        pkg_name <- environmentName(environment(val))
        if (pkg_name == "base" || pkg_name == "package:base") next
      }
      captured_globals[[var]] <- val
    }
  }
  return(digest::digest(captured_globals))
}

#' Helper to hash inputs
#' @keywords internal
.hash_inputs <- function(inputs, hash_file_paths, file_pattern = NULL, algo = "xxhash64") {
  hashed_inputs <- list()
  for (nm in names(inputs)) {
    val <- inputs[[nm]]
    
    is_file <- FALSE
    if (is.character(val) && length(val) == 1 && !is.na(val) && (file.exists(val) || dir.exists(val))) {
      is_file <- TRUE
    }

    # Check for vector of file paths (length > 1)
    is_file_vector <- FALSE
    if (is.character(val) && length(val) > 1) {
      file_mask <- !is.na(val) & (file.exists(val) | dir.exists(val))
      if (any(file_mask)) is_file_vector <- TRUE
    }

    if (is_file) {
      norm_p <- normalizePath(val, winslash = "/", mustWork = FALSE)
      if (hash_file_paths) {
         hashed_inputs[[nm]] <- norm_p
      } else {
         hashed_inputs[[nm]] <- .replace_paths_with_hashes(norm_p, file_pattern, algo)
      }
    } else if (is_file_vector) {
      if (hash_file_paths) {
        hashed_inputs[[nm]] <- vapply(val, function(el) {
          if (!is.na(el) && (file.exists(el) || dir.exists(el))) {
            normalizePath(el, winslash = "/", mustWork = FALSE)
          } else {
            el
          }
        }, character(1))
      } else {
        hashed_inputs[[nm]] <- .replace_paths_with_hashes(val, file_pattern, algo)
      }
    } else {
       if (hash_file_paths) {
         hashed_inputs[[nm]] <- val
       } else {
         hashed_inputs[[nm]] <- .replace_paths_with_hashes(val, file_pattern, algo)
       }
    }
  }
  return(digest::digest(hashed_inputs, algo = algo))
}


# -------------------------------------------------------------------------
# 4. Main Decorator
# -------------------------------------------------------------------------

#' Default Cache Directory
#' @export
cacheR_default_dir <- function() {
  getOption("cacheR.dir", default = file.path(tempdir(), "cacheR"))
}


#' Create a caching decorator
#' @export
cacheFile <- function(cache_dir       = NULL,
                      backend         = getOption("cacheR.backend", "rds"),
                      ignore_args     = NULL,
                      file_pattern    = NULL,
                      env_vars        = NULL,
                      hash_file_paths = TRUE,
                      algo            = "xxhash64") decorator %@% function(f) {

  force(f)
    if (is.null(cache_dir)) cache_dir <- cacheR_default_dir()
    if (!dir.exists(cache_dir)) try(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE), silent=TRUE)
    cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
    backend   <- match.arg(backend, c("rds", "qs"))
    
    # Sync Graph from disk on init
    cacheTree_sync(cache_dir)
    ast_deps <- .scan_ast_deps(body(f))
    
    # The actual wrapped function
    wrapper <- function(..., .load = TRUE) {
      # --- 1. CAPTURE & CANONICALIZE ARGUMENTS ---
      
      # A. Get the definition-based args (Standard args + Defaults)
      # We pass sys.call() to get the exact expression used to call this wrapper
      # parent.frame() is where the arguments (like 'x') exist.
      canon_args <- .get_canonical_values(f, sys.call(), parent.frame())
      
      # B. Handle Dots (...) Explicitly
      # .get_canonical_values strips '...' because they are hard to eval from call.
      # But inside this wrapper, 'list(...)' works perfectly.
      dots_values <- list(...)
      
      # Combine them: Named Defaults + Explicit Dots
      # This represents the "True Data" passed to the function.
      input_values <- c(canon_args, dots_values)
      
      if (!is.null(ignore_args)) {
        input_values <- input_values[ !names(input_values) %in% ignore_args ]
      }

      # C. Hash Inputs (Ignoring variable names, respecting content)
      input_hash <- .hash_inputs(input_values, hash_file_paths, file_pattern, algo)
      
      # --- 2. STATIC ANALYSIS (ENVIRONMENT & OPS) ---
      env_hash <- .get_scoped_env_hash(f)
      
      pkg_versions <- .get_pkg_versions(ast_deps$pkgs)
      current_envs <- if (!is.null(env_vars)) as.list(Sys.getenv(sort(env_vars), unset = NA)) else NULL
      current_opts <- if (length(ast_deps$opts) > 0) options()[ast_deps$opts] else NULL
      if (!is.null(current_opts)) current_opts <- current_opts[order(names(current_opts))]
      
      dir_states_key <- NULL
      if (hash_file_paths) {
        paths_to_hash <- unique(.extract_paths_recursively(input_values))
        if (length(paths_to_hash) > 0) {
           local_hasher <- function(p) .get_path_hash(p, file_pattern = file_pattern, algo = algo)
           dir_hashes <- vapply(paths_to_hash, local_hasher, character(1L))
           dir_states_key <- dir_hashes[order(names(dir_hashes))]
        }
      }

      # --- 3. COMPUTE FINAL KEY ---
      body_hash <- digest::digest(body(f), algo = algo)
      
      hashlist <- list(
        input_hash = input_hash, 
        env_hash = env_hash, 
        body_hash = body_hash,
        pkgs = pkg_versions,
        sys_envs = current_envs,
        sys_opts = current_opts,
        dir_states = dir_states_key
      )
      
      master_key <- digest::digest(hashlist, algo = algo)
      
      # Determine Filename
      fname_raw <- tryCatch(as.character(sys.call()[[1]]), error = function(e) "anon")
      if (length(fname_raw) > 1) fname_raw <- "anon" # handle complex calls
      fname <- gsub("[^a-zA-Z0-9_]", "_", fname_raw)
      if (nchar(fname) > 50) fname <- substring(fname, 1, 50)
      
      outfile <- file.path(cache_dir, paste(fname, master_key, backend, sep = "."))
      
      # --- 4. PERSISTENCE LAYER ---
      
      # Register Graph Node
      node_id <- paste(fname, master_key, sep = "_")
      .register_node(node_id, "function", label = fname, code = paste(deparse(sys.call()), collapse=" "), cache_dir = cache_dir)
      
      if (length(.graph_cache$call_stack) > 0) {
        parent_id <- tail(.graph_cache$call_stack, 1)
        .register_edge(from = parent_id, to = node_id, cache_dir = cache_dir)
      }
      
      .graph_cache$call_stack <- c(.graph_cache$call_stack, node_id)
      on.exit(.graph_cache$call_stack <- head(.graph_cache$call_stack, -1), add = TRUE)
      
      # Track Input Files
      input_paths <- unique(.extract_paths_recursively(input_values))
      for (p in input_paths) track_file(p, cache_dir = cache_dir) 

      # Hook
      if (exists(".cacheTree_register_node", mode = "function")) .cacheTree_register_node(node_id, fname, master_key, outfile)
      
      # --- 5. CHECK CACHE ---
      if (.load && file.exists(outfile)) {
        cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
        if (!is.null(cached_obj)) {
          if (is.list(cached_obj) && "value" %in% names(cached_obj)) {
            if (isTRUE(cached_obj$invisible)) return(invisible(cached_obj$value))
            return(cached_obj$value)
          }
          return(cached_obj)
        }
      }
      
      # --- 6. EXECUTE (MISS) ---
      
      # Setup file monitoring for warning
      paths_to_monitor <- input_paths
      file_snapshots <- character()
      if (length(paths_to_monitor) > 0) {
         infos <- file.info(paths_to_monitor)
         file_snapshots <- paste(infos$size, unclass(infos$mtime))
         names(file_snapshots) <- paths_to_monitor
      }
      
      # RUN FUNCTION (capture visibility for invisible() preservation)
      dat_vis <- withVisible(f(...))
      dat <- dat_vis$value
      dat_invisible <- !dat_vis$visible

      # Check for file side-effects
      if (length(paths_to_monitor) > 0) {
         new_infos <- file.info(paths_to_monitor)
         new_snapshots <- paste(new_infos$size, unclass(new_infos$mtime))
         changed_files <- paths_to_monitor[file_snapshots != new_snapshots]
         if (length(changed_files) > 0) {
           warning(sprintf("cacheR: [WARNING] Function modified argument files during execution: %s.", paste(basename(changed_files), collapse=", ")))
         }
      }
      
      # --- 7. SAVE RESULT ---
      full_meta <- c(hashlist, list(
         fname = fname, 
         args_hash = master_key, 
         created = Sys.time(),
         cache_file = normalizePath(outfile, winslash = "/", mustWork = FALSE),
         cache_dir  = normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
         args_values = input_values
      ))
      
      save_obj <- list(value=dat, meta=full_meta)
      if (dat_invisible) save_obj$invisible <- TRUE
      do_save <- function() .atomic_save(save_obj, outfile, backend)
      
      if (requireNamespace("filelock", quietly = TRUE)) {
        lock <- tryCatch(filelock::lock(paste0(outfile, ".lock"), timeout = 5000), error = function(e) NULL)
        if (!is.null(lock)) {
          on.exit(filelock::unlock(lock), add = TRUE)
          # Double check if someone else wrote it while we waited
          if (.load && file.exists(outfile)) {
             cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
             if (!is.null(cached_obj)) {
               if (is.list(cached_obj) && "value" %in% names(cached_obj)) {
                 if (isTRUE(cached_obj$invisible)) return(invisible(cached_obj$value))
                 return(cached_obj$value)
               }
               return(cached_obj)
             }
          }
          do_save()
        } else { do_save() }
      } else { do_save() }

      if (dat_invisible) invisible(dat) else dat
    }
    
    return(wrapper)
}

# -------------------------------------------------------------------------
# 5. Export / Helpers
# -------------------------------------------------------------------------

#' Prune old cache files
#' @export
cachePrune <- function(cache_dir, days_old = 30) {
  if (!dir.exists(cache_dir)) return(invisible(NULL))
  files <- list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE)
  if (length(files) == 0) return(invisible(NULL))
  
  infos <- file.info(files)
  cutoff <- Sys.time() - (days_old * 24 * 60 * 60)
  
  to_delete <- files[infos$mtime < cutoff]
  if (length(to_delete) > 0) {
    unlink(to_delete)
    message(sprintf("Pruned %d files older than %d days.", length(to_delete), days_old))
  }
}

#' Export to targets
#' @export
export_targets_file <- function(path = "_targets.R") {
  nodes_env <- .graph_cache$nodes
  nodes_list <- as.list(nodes_env)
  
  header <- c("library(targets)", "library(tarchetypes)", "tar_option_set(packages = c('base'))", "", "list(")
  targets_list <- c()
  
  file_nodes <- Filter(function(x) x$type == "file", nodes_list)
  # Sort for deterministic output
  if (length(file_nodes) > 0) file_nodes <- file_nodes[order(names(file_nodes))]
  
  for (f in file_nodes) {
    t_name <- paste0("file_", gsub("[^a-zA-Z0-9_]", "_", basename(f$id)))
    # For file nodes, command is the path
    targets_list <- c(targets_list, sprintf("  tar_target(name = %s, command = \"%s\", format = \"file\")", t_name, f$id))
  }
  
  func_nodes <- Filter(function(x) x$type == "function", nodes_list)
  if (length(func_nodes) > 0) func_nodes <- func_nodes[order(names(func_nodes))]
  
  for (fn in func_nodes) {
    cmd <- if (!is.null(fn$code)) fn$code else "NULL"
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", fn$label)
    targets_list <- c(targets_list, sprintf("  tar_target(name = %s, command = { %s })", safe_name, cmd))
  }
  
  writeLines(c(header, paste(targets_list, collapse = ",\n\n"), ")"), path)
  message(sprintf("Exported %d targets to %s", length(targets_list), path))
}

.atomic_save <- function(object, path, backend) {
  tmp_path <- paste0(path, ".tmp.", paste(sample(c(letters, 0:9), 8, replace=TRUE), collapse=""))
  tryCatch({
    if (backend == "qs") { if (!requireNamespace("qs", quietly=TRUE)) stop("qs required"); qs::qsave(object, tmp_path) } 
    else { saveRDS(object, tmp_path) }
    file.rename(tmp_path, path)
  }, error = function(e) {
    if(file.exists(tmp_path)) unlink(tmp_path)
    warning(sprintf("cacheR: failed to save cache file '%s': %s", basename(path), conditionMessage(e)), call. = FALSE)
  })
}
.safe_load <- function(path, backend) {
  if (backend == "qs") { if (!requireNamespace("qs", quietly=TRUE)) stop("qs required"); qs::qread(path) } 
  else { readRDS(path) }
}
.get_recursive_closure_hash <- function(obj, visited=NULL, algo="xxhash64", version_checker=utils::packageVersion) {
   if(is.null(visited)) visited <- new.env(parent=emptyenv())
   if(is.function(obj)) {
     obj_id <- digest::digest(obj, algo = algo)
     if (exists(obj_id, envir = visited)) return(visited[[obj_id]])
     visited[[obj_id]] <- "RECURSION_CYCLE"
     
     fn_env <- environment(obj)
     if (is.environment(fn_env) && isNamespace(fn_env)) {
       pkg_name <- getNamespaceName(fn_env)
       pkg_ver <- tryCatch(as.character(version_checker(pkg_name)), error = function(e) "unknown")
       final_hash <- digest::digest(list(package = pkg_name, version = pkg_ver), algo = algo)
       visited[[obj_id]] <- final_hash
       return(final_hash)
     }

     body_hash <- digest::digest(list(formals(obj), body(obj)), algo=algo)
     globals   <- codetools::findGlobals(obj, merge = TRUE)
     dep_hashes <- lapply(globals, function(var_name) {
      val <- try(get(var_name, envir = fn_env), silent = TRUE)
      if (inherits(val, "try-error")) return(NULL)
      .get_recursive_closure_hash(val, visited, algo = algo, version_checker = version_checker)
     })
     final_hash <- digest::digest(list(body = body_hash, deps = dep_hashes), algo = algo)
     visited[[obj_id]] <- final_hash
     return(final_hash)
   }
   
   if (is.environment(obj)) {
     if (isNamespace(obj) || identical(obj, .GlobalEnv)) return(digest::digest(environmentName(obj), algo=algo))
     obj_list <- as.list(obj)
     if (length(obj_list) > 0) {
       obj_list <- obj_list[order(names(obj_list))]
       hashes <- lapply(obj_list, function(x) .get_recursive_closure_hash(x, visited, algo, version_checker))
       return(digest::digest(hashes, algo=algo))
     }
     return(digest::digest("empty_env", algo=algo))
   }
   
   if (is.list(obj)) {
     hashes <- lapply(obj, function(x) .get_recursive_closure_hash(x, visited, algo, version_checker))
     return(digest::digest(hashes, algo=algo))
   }
   
   return(digest::digest(.replace_paths_with_hashes(obj, algo=algo), algo=algo))
}