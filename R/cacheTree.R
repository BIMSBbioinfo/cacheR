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

.file_state_cache <- new.env(parent = emptyenv())

# ------------------------------------------------------------ #
probabilistic_file_hash <- function(path, block_size = 64 * 1024, n_blocks = 5, algo = "xxhash64") {
  if (!file.exists(path)) return(NA_character_)

  size <- file.info(path)$size
  con  <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  blocks <- list()
  # First block (Header is usually important)
  blocks[[1]] <- readBin(con, "raw", block_size)

  if (size > block_size) {
    max_offset <- max(size - block_size, 1)
    
    # --- CRITICAL FIX START ---
    # Ensure deterministic sampling based on file size + path
    # We scramble the size/path to get a seed
    seed_val <- as.integer(charToRaw(paste0(path, size))) 
    set.seed(sum(seed_val)) 
    # --- CRITICAL FIX END ---
    
    for (i in seq_len(n_blocks)) {
      offset <- sample.int(max_offset, 1)
      seek(con, offset, "start")
      blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
    }
  }

  # Last block (Footer is usually important)
  if (size > block_size) {
    seek(con, max(size - block_size, 0), "start")
    blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  }

  bytes <- do.call(c, blocks)
  digest::digest(bytes, algo = algo)
}

fast_file_hash <- function(
    path,
    block_size = 64 * 1024,
    n_blocks   = 5,
    algo       = "xxhash64"
) {
  info <- file.info(path)
  if (is.na(info$size)) return(NA_character_)

  fp <- paste(info$size, unclass(info$mtime), sep = "|")

  prev <- .file_state_cache[[path]]
  if (!is.null(prev) && identical(prev$fp, fp)) {
    return(prev$hash)
  }

  h <- probabilistic_file_hash(
    path,
    block_size = block_size,
    n_blocks   = n_blocks,
    algo       = algo
  )

  .file_state_cache[[path]] <- list(fp = fp, hash = h)
  h
}

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
    fh[np] <- fast_file_hash(path)
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