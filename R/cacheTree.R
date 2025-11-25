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

cacheTree_nodes <- function() {
  # returns a named list of all nodes
  ids <- ls(.cacheTree_env$graph, all.names = TRUE)
  setNames(
    lapply(ids, function(k) .cacheTree_env$graph[[k]]),
    ids
  )
}

cacheTree_for_file <- function(path) {
  np <- tryCatch(normalizePath(path, mustWork = FALSE),
                 error = function(e) path)
  nodes <- cacheTree_nodes()
  keep <- vapply(nodes, function(n) np %in% n$files, logical(1))
  nodes[keep]
}

cacheTree_changed_files <- function() {
  nodes <- cacheTree_nodes()
  out <- list()
  for (id in names(nodes)) {
    n  <- nodes[[id]]
    fh <- n$file_hashes
    if (!length(fh)) next
    changed <- logical(length(fh))
    for (i in seq_along(fh)) {
      p        <- names(fh)[i]
      old_hash <- fh[[i]]
      if (!file.exists(p)) {
        changed[i] <- TRUE
      } else {
        new_hash <- digest::digest(file = p, algo = "md5")
        changed[i] <- !identical(old_hash, new_hash)
      }
    }
    if (any(changed)) {
      out[[id]] <- list(
        node          = n,
        changed_files = names(fh)[changed]
      )
    }
  }
  out
}

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

probabilistic_file_hash <- function(
    path,
    block_size = 64 * 1024,
    n_blocks   = 5,
    algo       = "xxhash64"
) {
  if (!file.exists(path)) return(NA_character_)
  
  size <- file.info(path)$size
  con  <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  
  blocks <- list()
  
  # First block
  blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
  
  # Random blocks
  if (size > block_size) {
    max_offset <- max(size - block_size, 1)
    for (i in seq_len(n_blocks)) {
      offset <- sample.int(max_offset, 1)
      seek(con, offset, "start")
      blocks[[length(blocks) + 1L]] <- readBin(con, "raw", block_size)
    }
  }
  
  # Last block
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




# --- The caching decorator factory ---------------------------------------

cacheFile <- function(inpath) decorator %@% function(f) {
  
  library(digest)
  
  # capture formal arguments and body as in your original
  argnames <- head(as.list(args(as.list(environment())[[1]])), -1)
  fbody    <- lapply(as.list(body(f)), as.character)
  
  function(..., .load = TRUE, .anames = argnames, .fbody = fbody) {
    
    # ---- reconstruct the call & arguments (your original logic) ---------
    fcall <- as.list(match.call())
    
    fname <- fcall[[1]]
    args  <- fcall[-1]
    
    if (!is.null(names(args)) && any(names(args) == ".load"))
      args <- args[names(args) != ".load"]
    
    if (!is.null(names(args))) {
      named_args <- setdiff(names(args), "")
      if (!is.null(named_args)) {
        for (i in named_args)
          .anames[[i]] <- args[[i]]
      }
      
      pos_args <- which(names(args) == "")
      if (length(pos_args) > 0) {
        for (i in pos_args)
          .anames[[i]] <- args[[i]]
      }
    } else {
      for (i in seq_along(args))
        .anames[[i]] <- args[[i]]
    }
    
    .dotind <- names(.anames) == "..."
    if (any(.dotind)) {
      .anames <- .anames[!.dotind]
    }
    
    if (length(args) > 0) {
      for (i in seq_along(.anames)) {
        if (is.call(.anames[[i]]) || is.name(.anames[[i]])) {
          val <- eval(.anames[[i]], envir = parent.frame())
          if (is.null(val)) val <- list(NULL)
          .anames[[i]] <- val
        }
      }
    }
    
    # ---- compute hash & output path -------------------------------------
    hashlist  <- list(anames = .anames, body = .fbody)
    args_hash <- digest::digest(hashlist, algo = "md5")
    message(args_hash)
    
    outfile <- file.path(
      inpath,
      paste(as.character(fname), args_hash, "rds", sep = ".")
    )
    
    # ---- register node + manage call stack ------------------------------
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
    
    # ---- caching logic (unchanged semantics) ----------------------------
    if (.load && file.exists(outfile)) {
      message(paste0(fname, ": Returning loaded data ..."))
      message(outfile)
      readRDS(outfile)$dat
    } else {
      message(paste0(fname, ": Running function ..."))
      dat <- f(...)
      
      saveRDS(list(dat = dat, args = .anames, body = .fbody), outfile)
      dat
    }
  }
}

cacheTree_reset()

cache_dir <- file.path(tempdir(), "cache_manual_1")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

runs <- 0

simple <- cacheFile(cache_dir) %@% function(x) {
  runs <<- runs + 1
  x * 2
}

simple(10)

nodes <- cacheTree_nodes()
print(names(nodes))
print(nodes)

names(nodes)
nodes[[1]]$fname

cacheTree_reset()

cache_dir <- file.path(tempdir(), "cache_manual_2")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

inner_fun <- cacheFile(cache_dir) %@% function(x) {
  x + 1
}

outer_fun <- cacheFile(cache_dir) %@% function(x) {
  inner_fun(x) * 2
}

outer_fun(3)

nodes <- cacheTree_nodes()
print(names(nodes))

outer_id <- grep("^outer_fun:", names(nodes), value = TRUE)
inner_id <- grep("^inner_fun:", names(nodes), value = TRUE)

cat("outer_id:", outer_id, "\n")
cat("inner_id:", inner_id, "\n")

if (length(outer_id)) print(nodes[[outer_id]])
if (length(inner_id)) print(nodes[[inner_id]])
