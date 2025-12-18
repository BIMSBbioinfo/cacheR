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

#' Register a Node
#' @keywords internal
.register_node <- function(id, type, label = id, code = NULL) {
  if (!exists(id, envir = .graph_cache$nodes)) {
    assign(id, list(
      id = id, 
      type = type, 
      label = label, 
      code = code,
      files = character(),
      file_hashes = list(),
      children = character(),
      parents = character()
    ), envir = .graph_cache$nodes)
  }
}

#' Register an Edge
#' @keywords internal
.register_edge <- function(from, to) {
  edge_id <- paste(from, to, sep = "->")
  if (!exists(edge_id, envir = .graph_cache$edges)) {
    assign(edge_id, list(from = from, to = to), envir = .graph_cache$edges)
    
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
  }
}

#' Find nodes associated with a specific file path
#' @export
cacheTree_for_file <- function(path) {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  all_nodes <- cacheTree_nodes()
  # Filter nodes where 'path' is in the files list
  Filter(function(x) path %in% x$files, all_nodes)
}

#' Save the current graph to disk
#' @export
cacheTree_save <- function(file) {
  data <- list(
    nodes = as.list(.graph_cache$nodes),
    edges = as.list(.graph_cache$edges)
  )
  saveRDS(data, file)
}

#' Load a graph from disk
#' @export
cacheTree_load <- function(file) {
  data <- readRDS(file)
  cacheTree_reset()
  list2env(data$nodes, envir = .graph_cache$nodes)
  list2env(data$edges, envir = .graph_cache$edges)
}

#' Explicitly Track a File Dependency
#' @export
track_file <- function(path) {
  path_norm <- normalizePath(path, mustWork = FALSE, winslash = "/")
  
  if (dir.exists(path_norm)) {
    hash <- .get_path_hash(path_norm)
  } else {
    hash <- .fast_file_hash(path_norm)
  }
  
  # Register the file as a node
  .register_node(id = path_norm, type = "file", label = basename(path_norm))
  
  if (length(.graph_cache$call_stack) > 0) {
    active_node_id <- tail(.graph_cache$call_stack, 1)
    
    # Register Edge: Function -> File
    .register_edge(from = active_node_id, to = path_norm)
    
    # Update Function Node Metadata
    if (exists(active_node_id, envir = .graph_cache$nodes)) {
      node <- get(active_node_id, envir = .graph_cache$nodes)
      if (!path_norm %in% node$files) {
        node$files <- c(node$files, path_norm)
      }
      node$file_hashes[[path_norm]] <- hash
      assign(active_node_id, node, envir = .graph_cache$nodes)
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
.probabilistic_file_hash <- function(path, block_size = 64 * 1024, n_blocks = 5, algo = "xxhash64") {
  path <- normalizePath(path, mustWork = FALSE)
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  
  info <- file.info(path)
  size <- info$size
  if (is.na(size)) return(NA_character_)
  
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(NA_character_)
  
  on.exit(close(con), add = TRUE)
  blocks <- list()
  blocks[[1]] <- readBin(con, "raw", block_size)
  
  if (size > block_size) {
    max_offset <- max(size - block_size, 1)
    seed_raw <- charToRaw(digest::digest(paste0(path, size), algo = "crc32"))
    seed_int <- sum(as.integer(seed_raw))
    for (i in seq_len(n_blocks)) {
      offset <- (seed_int * i) %% max_offset + 1L
      seek(con, offset, "start")
      blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
    }
    seek(con, max(size - block_size, 0), "start")
    blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  }
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
    return(x)
  } else if (is.list(x)) {
    return(lapply(x, .replace_paths_with_hashes, file_pattern=file_pattern, algo=algo))
  } else if (is.environment(x) && !isNamespace(x) && !identical(x, .GlobalEnv)) {
    return(lapply(as.list(x), .replace_paths_with_hashes, file_pattern=file_pattern, algo=algo))
  } else if (inherits(x, "connection") || inherits(x, "DBIConnection")) {
     p <- .get_connection_path(x)
     if (!is.null(p)) return(.fast_file_hash(p, algo = algo))
  }
  return(x)
}

#' Scan AST for Dependencies
#' @keywords internal
.scan_ast_deps <- function(expr) {
  pkgs <- character(); opts <- character()
  if (is.call(expr)) {
    fn_sym <- expr[[1]]
    if (is.symbol(fn_sym) && (as.character(fn_sym) %in% c("::", ":::"))) pkgs <- c(pkgs, as.character(expr[[2]]))
    if (is.symbol(fn_sym) && as.character(fn_sym) == "getOption") {
      val <- tryCatch(eval(expr[[2]]), error = function(e) NULL)
      if (is.character(val)) opts <- c(opts, val)
    }
    for (i in seq_along(expr)) { res <- .scan_ast_deps(expr[[i]]); pkgs <- c(pkgs, res$pkgs); opts <- c(opts, res$opts) }
  } else if (is.expression(expr) || is.list(expr)) {
    for (i in seq_along(expr)) { res <- .scan_ast_deps(expr[[i]]); pkgs <- c(pkgs, res$pkgs); opts <- c(opts, res$opts) }
  }
  return(list(pkgs = unique(pkgs), opts = unique(opts)))
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
cacheFile <- function(cache_dir     = NULL,
                      backend       = getOption("cacheR.backend", "rds"),
                      ignore_args   = NULL,
                      file_pattern  = NULL,
                      env_vars      = NULL,
                      hash_file_paths = TRUE,
                      algo          = "xxhash64") decorator %@% function(f) {
  
  if (is.null(cache_dir)) cache_dir <- cacheR_default_dir()
  if (!dir.exists(cache_dir)) try(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE), silent=TRUE)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  backend   <- match.arg(backend, c("rds", "qs"))
  
  ast_deps <- .scan_ast_deps(body(f))
  
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
    # 1. Parse Arguments
    # ---------------------------------------------------------- #
    call_matched <- match.call(definition = f, expand.dots = TRUE)
    args_exprs   <- as.list(call_matched)[-1]
    args_values  <- lapply(args_exprs, function(expr) eval(expr, envir = invoke_env))
    
    f_defs <- formals(f); f_defs <- f_defs[names(f_defs) != "..."]
    eval_env <- new.env(parent = invoke_env)
    if (!is.null(names(args_values))) {
       valid <- nzchar(names(args_values))
       if(any(valid)) list2env(args_values[valid], envir = eval_env)
    }
    for (nm in names(f_defs)) {
      if (!nm %in% names(args_values)) {
        def_expr <- f_defs[[nm]]
        if (!is.symbol(def_expr) || as.character(def_expr) != "") {
           if (!nm %in% names(args_exprs)) args_exprs[[nm]] <- def_expr
           val <- tryCatch(eval(def_expr, envir = eval_env), error = function(e) NULL)
           args_values[[nm]] <- val
           assign(nm, val, envir = eval_env)
        }
      }
    }
    
    if (!is.null(ignore_args)) {
      args_values <- args_values[ !names(args_values) %in% ignore_args ]
      args_exprs <- args_exprs[ !names(args_exprs) %in% ignore_args ]
    }
    if (length(args_values) > 0) {
      ord <- order(names(args_values))
      args_values <- args_values[ord]
      if(length(args_exprs) == length(args_values)) args_exprs <- args_exprs[ord]
    }
    
    # ---------------------------------------------------------- #
    # 2. Compute Hash
    # ---------------------------------------------------------- #
    closure_hash <- .get_recursive_closure_hash(f, algo = algo)
    
    args_for_hash <- list()
    if (length(args_values) > 0) {
      for (nm in names(args_values)) {
        val <- args_values[[nm]]
        expr <- if (nm %in% names(args_exprs)) args_exprs[[nm]] else NULL
        
        is_file <- FALSE
        if (is.character(val) && length(val) == 1 && !is.na(val) && (file.exists(val) || dir.exists(val))) {
          is_file <- TRUE
        }
        
        if (is_file) {
          norm_p <- normalizePath(val, winslash = "/", mustWork = FALSE)
          if (hash_file_paths) {
             args_for_hash[[nm]] <- norm_p
          } else {
             args_for_hash[[nm]] <- .replace_paths_with_hashes(norm_p, file_pattern, algo)
          }
        } else {
          if (hash_file_paths) {
             args_for_hash[[nm]] <- list(val = val, expr = expr)
          } else {
             args_for_hash[[nm]] <- list(val = .replace_paths_with_hashes(val, file_pattern, algo), expr = expr)
          }
        }
      }
    }
    
    dir_states_key <- NULL
    if (hash_file_paths) {
      paths_to_hash <- unique(.extract_paths_recursively(args_for_hash))
      if (length(paths_to_hash) > 0) {
         local_hasher <- function(p) .get_path_hash(p, file_pattern = file_pattern, algo = algo)
         dir_hashes <- vapply(paths_to_hash, local_hasher, character(1L))
         dir_states_key <- dir_hashes[order(names(dir_hashes))]
      }
    }
    
    pkg_versions <- .get_pkg_versions(ast_deps$pkgs)
    current_envs <- if (!is.null(env_vars)) as.list(Sys.getenv(sort(env_vars), unset = NA)) else NULL
    current_opts <- if (length(ast_deps$opts) > 0) options()[ast_deps$opts] else NULL
    if (!is.null(current_opts)) current_opts <- current_opts[order(names(current_opts))]
    
    hashlist <- list(
      closure = closure_hash, pkgs = pkg_versions, envs = current_envs, 
      opts = current_opts, args = args_for_hash, dir_states = dir_states_key
    )
    
    args_hash <- digest::digest(hashlist, algo = algo)
    outfile   <- file.path(cache_dir, paste(fname, args_hash, backend, sep = "."))
    
    # ---------------------------------------------------------- #
    # 3. Register Graph Node & Stack
    # ---------------------------------------------------------- #
    # Uses static function name for Targets compatibility
    node_id <- fname 
    
    .register_node(node_id, "function", label = fname, code = paste(deparse(full_call), collapse=" "))
    
    if (length(.graph_cache$call_stack) > 0) {
      parent_id <- tail(.graph_cache$call_stack, 1)
      .register_edge(from = parent_id, to = node_id)
    }
    
    .graph_cache$call_stack <- c(.graph_cache$call_stack, node_id)
    on.exit(.graph_cache$call_stack <- head(.graph_cache$call_stack, -1), add = TRUE)
    
    # 4. Detect Input Files (AND TRACK THEM IN GRAPH)
    input_paths <- unique(.extract_paths_recursively(args_values))
    for (p in input_paths) track_file(p) 

    # D. Hooks
    if (exists(".cacheTree_register_node", mode = "function")) .cacheTree_register_node(node_id, fname, args_hash, outfile)

    # E. Load
    if (.load && file.exists(outfile)) {
      cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
      if (!is.null(cached_obj)) {
        if (is.list(cached_obj) && "value" %in% names(cached_obj)) return(cached_obj$value)
        return(cached_obj)
      }
    }
    
    # F. Miss & Guard (Warning Only)
    paths_to_monitor <- unique(.extract_paths_recursively(args_values))
    file_snapshots <- character()
    if (length(paths_to_monitor) > 0) {
       infos <- file.info(paths_to_monitor)
       file_snapshots <- paste(infos$size, unclass(infos$mtime))
       names(file_snapshots) <- paths_to_monitor
    }
    
    dat <- f(...)
    
    if (length(paths_to_monitor) > 0) {
       new_infos <- file.info(paths_to_monitor)
       new_snapshots <- paste(new_infos$size, unclass(new_infos$mtime))
       changed_files <- paths_to_monitor[file_snapshots != new_snapshots]
       if (length(changed_files) > 0) {
         warning(sprintf("cacheR: [WARNING] Function modified argument files during execution: %s.", paste(basename(changed_files), collapse=", ")))
       }
    }
    
    # G. Save
    full_meta <- c(hashlist, list(
       fname = fname, 
       args_hash = args_hash, 
       created = Sys.time(),
       cache_file = normalizePath(outfile, winslash = "/", mustWork = FALSE),
       cache_dir  = normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
       args_values = args_values,
       args_exprs = args_exprs
    ))
    do_save <- function() .atomic_save(list(value=dat, meta=full_meta), outfile, backend)
    
    if (requireNamespace("filelock", quietly = TRUE)) {
      lock <- tryCatch(filelock::lock(paste0(outfile, ".lock"), timeout = 5000), error = function(e) NULL)
      if (!is.null(lock)) {
        on.exit(filelock::unlock(lock), add = TRUE)
        if (.load && file.exists(outfile)) {
           cached_obj <- tryCatch(.safe_load(outfile, backend), error = function(e) NULL)
           if (!is.null(cached_obj)) return(if(is.list(cached_obj) && "value" %in% names(cached_obj)) cached_obj$value else cached_obj)
        }
        do_save()
      } else { do_save() }
    } else { do_save() }
    
    dat
  }
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
  }, error = function(e) { if(file.exists(tmp_path)) unlink(tmp_path) })
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