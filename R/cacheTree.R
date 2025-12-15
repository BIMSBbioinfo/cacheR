# cacheTree.R --------------------------------------------------------------

# Global environment to store analysis graph + call stack
.cacheTree_env <- new.env(parent = emptyenv())
.cacheTree_env$call_stack <- character()
.cacheTree_env$graph      <- new.env(parent = emptyenv())

# --- Helpers to manage the graph -----------------------------------------

.cacheTree_current_node <- function() {
  s <- .cacheTree_env$call_stack
  if (!length(s)) return(NA_character_)
  s[[length(s)]]
}

.cacheTree_register_node <- function(node_id, fname, args_hash, outfile) {
  parent <- .cacheTree_current_node()

  node <- .cacheTree_env$graph[[node_id]]
  if (is.null(node)) {
    node <- list(
      id          = node_id,
      fname       = as.character(fname),
      hash        = args_hash,
      outfile     = outfile,
      parents     = character(),
      children    = character(),
      files       = character(),
      file_hashes = character(),  # named character vector
      created     = Sys.time()
    )
  }

  # Link to parent, if any
  if (!is.na(parent)) {
    node$parents <- unique(c(node$parents, parent))

    parent_node <- .cacheTree_env$graph[[parent]]
    if (is.null(parent_node)) {
      parent_node <- list(
        id          = parent,
        fname       = NA_character_,
        hash        = NA_character_,
        outfile     = NA_character_,
        parents     = character(),
        children    = character(),
        files       = character(),
        file_hashes = character(),
        created     = Sys.time()
      )
    }
    parent_node$children <- unique(c(parent_node$children, node_id))
    .cacheTree_env$graph[[parent]] <- parent_node
  }

  .cacheTree_env$graph[[node_id]] <- node
}

# Public helpers -----------------------------------------------------------
#' Return cache tree nodes
#'
#' Returns a list/data.frame describing all nodes currently recorded
#' in the cache tree (each cached call instance).
#'
#' @return An object representing nodes (e.g. a data.frame with
#'   `id`, `fn_label`, `key`, `cache_file`, etc.).
#' @export
cacheTree_nodes <- function() {
  # returns a named list of all nodes
  ids <- ls(.cacheTree_env$graph, all.names = TRUE)
  setNames(
    lapply(ids, function(k) .cacheTree_env$graph[[k]]),
    ids
  )
}

# ------------------------------------------------------------ #
cacheTree_for_file <- function(path) {
  np <- tryCatch(normalizePath(path, mustWork = FALSE),
                 error = function(e) path)
  nodes <- cacheTree_nodes()
  keep <- vapply(nodes, function(n) np %in% n$files, logical(1))
  nodes[keep]
}

# ------------------------------------------------------------ #
#' Reset the cache tree state
#'
#' Clears all recorded parent-child relationships between cached calls.
#' Does not (by default) delete files from disk (that should be handled
#' separately, e.g. by cleaning `cache_dir`).
#'
#' @export
cacheTree_reset <- function() {
  .cacheTree_env$call_stack <- character()
  .cacheTree_env$graph      <- new.env(parent = emptyenv())
  invisible(TRUE)
}

cacheTree_save <- function(path) {
  # Save a serializable representation (named list of nodes)
  saveRDS(cacheTree_nodes(), path)
  invisible(path)
}

cacheTree_load <- function(path) {
  graph_list <- readRDS(path)
  .cacheTree_env$graph <- new.env(parent = emptyenv())
  for (id in names(graph_list)) {
    .cacheTree_env$graph[[id]] <- graph_list[[id]]
  }
  .cacheTree_env$call_stack <- character()
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# File fingerprinting + probabilistic hashing + metadata cache
# -------------------------------------------------------------------------
# Strategy:
#   - For each file path, store:
#       fp   = paste(size, mtime)
#       hash = sampled content hash
#   - On each call:
#       * If size+mtime unchanged → reuse old hash (no disk read)
#       * Else → recompute probabilistic hash from sampled blocks
# -------------------------------------------------------------------------

# ---------------- file hashing (metadata + probabilistic) -----------------


# ------------------------------------------------------------ #
track_file <- function(path) {
  node_id <- .cacheTree_current_node()
  if (is.na(node_id)) return(path)

  node <- .cacheTree_env$graph[[node_id]]
  if (is.null(node)) return(path)

  np <- tryCatch(
    normalizePath(path, mustWork = FALSE),
    error = function(e) path
  )

  node$files <- unique(c(node$files, np))

  fh <- node$file_hashes
  if (is.null(fh)) fh <- character()

  if (file.exists(path)) {
    fh[np] <- .fast_file_hash(path)
  } else {
    fh[np] <- NA_character_
  }

  node$file_hashes <- fh
  .cacheTree_env$graph[[node_id]] <- node

  path
}

# ------------------------------------------------------------ #
cacheTree_changed_files <- function() {
  nodes <- cacheTree_nodes()
  out   <- list()

  for (id in names(nodes)) {
    n  <- nodes[[id]]
    fh <- n$file_hashes
    if (!length(fh)) next

    cur_paths <- names(fh)
    changed   <- logical(length(fh))

    for (i in seq_along(cur_paths)) {
      p        <- cur_paths[[i]]
      old_hash <- fh[[i]]

      if (!file.exists(p)) {
        changed[i] <- TRUE
      } else {
        new_hash <- fast_file_hash(p)
        changed[i] <- !identical(old_hash, new_hash)
      }
    }

    if (any(changed)) {
      out[[id]] <- list(
        node          = n,
        changed_files = cur_paths[changed]
      )
    }
  }

  out
}


# ------------------------------------------------------------ #
cacheR_default_dir <- function() {
  d <- getOption("cacheR.dir", file.path(getwd(), ".cacheR"))

  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  d
}

# ------------------------------------------------------------ #
#' Prune old cache files
#' @param days_old Delete files not accessed in X days
#' @export
cachePrune <- function(cache_dir, days_old = 30) {
  files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.(rds|qs)$")
  if (length(files) == 0) return(invisible())
  
  # Get file info
  info <- file.info(files)
  
  # Calculate age based on 'atime' (access time) if supported, else 'mtime'
  # Note: atime updates depend on OS/filesystem settings (noatime mount option)
  now <- Sys.time()
  age <- difftime(now, info$mtime, units = "days")
  
  to_delete <- files[age > days_old]
  
  if (length(to_delete) > 0) {
    message(sprintf("Deleting %d old cache files...", length(to_delete)))
    unlink(to_delete)
  }
}


#--- Internal Helper: Normalize Path with Forward Slashes ---
.norm_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# --- Cache Info & List Utilities ------------------------------------------

#' Retrieve metadata from a cached file
#' 
#' @param path Path to the .rds file
#' @return A list containing $value and $meta
#' @export
cacheInfo <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  
  obj <- readRDS(path)
  
  # Check if it matches the new metadata structure
  is_new_format <- is.list(obj) && 
                   all(c("value", "meta") %in% names(obj)) && 
                   is.list(obj$meta)
  
  if (is_new_format) {
    return(obj)
  } else {
    # Legacy handling: wrap raw value in structure
    return(list(
      value = obj,
      meta = list(
        fname       = NA_character_,
        args        = list(),
        args_hash   = NA_character_,
        cache_file  = .norm_path(path),
        cache_dir   = .norm_path(dirname(path)),
        created     = file.info(path)$mtime,
        legacy      = TRUE
      )
    ))
  }
}

#' List contents of a cache directory
#'
#' @param cache_dir Directory to scan
#' @return A data.frame of cache files and metadata
#' @export
cacheList <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    return(data.frame(
      file = character(), 
      fname = character(), 
      created = as.POSIXct(character()), 
      size_bytes = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
  
  if (length(files) == 0) {
    return(data.frame(
      file = character(), 
      fname = character(), 
      created = as.POSIXct(character()), 
      size_bytes = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract info from each file
  rows <- lapply(files, function(f) {
    info <- tryCatch(cacheInfo(f), error = function(e) NULL)
    file_stat <- file.info(f)
    
    if (is.null(info)) {
      fname <- NA_character_
    } else {
      fname <- if (!is.null(info$meta$fname)) info$meta$fname else NA_character_
    }
    
    data.frame(
      file       = basename(f),
      fname      = fname,
      created    = file_stat$mtime,
      size_bytes = file_stat$size,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
