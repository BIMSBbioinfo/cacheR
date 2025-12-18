# =========================================================================
# cacheR: Graph Visualization & Management Utilities
# =========================================================================

# -------------------------------------------------------------------------
# 1. Graph State Inspection
# -------------------------------------------------------------------------

#' Get all nodes
#' @export
cacheTree_nodes <- function() {
  # Always return a list to support iteration and subsetting functions like Filter
  as.list(.graph_cache$nodes)
}

#' Find nodes associated with a specific file path
#' @export
cacheTree_for_file <- function(path) {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  
  # Ensure all_nodes is a list
  all_nodes <- cacheTree_nodes()
  if (is.environment(all_nodes)) all_nodes <- as.list(all_nodes)
  
  # Filter nodes where 'path' is in the files list
  Filter(function(x) path %in% x$files, all_nodes)
}

#' Reset the cache tree state
#' @export
cacheTree_reset <- function() {
  cacheR_reset_graph() # Calls the internal reset defined in cacheFile.R
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# 2. Graph Persistence (Save/Load the visualization data)
# -------------------------------------------------------------------------

#' Save the execution graph
#' @export
cacheTree_save <- function(path) {
  saveRDS(list(
    nodes = .graph_cache$nodes,
    edges = .graph_cache$edges
  ), path)
  invisible(path)
}

#' Load an execution graph
#' @export
cacheTree_load <- function(path) {
  data <- readRDS(path)
  if (!all(c("nodes", "edges") %in% names(data))) stop("Invalid graph file")
  
  .graph_cache$nodes <- data$nodes
  .graph_cache$edges <- data$edges
  .graph_cache$call_stack <- character() # Reset stack on load
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# 3. File & Directory Management
# -------------------------------------------------------------------------

#' Get Default Cache Directory
#' @export
cacheR_default_dir <- function() {
  d <- getOption("cacheR.dir", file.path(getwd(), ".cacheR"))
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  d
}

#' Prune old cache files
#' @param cache_dir Directory to prune
#' @param days_old Delete files not accessed/modified in X days
#' @export
cachePrune <- function(cache_dir = cacheR_default_dir(), days_old = 30) {
  files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.(rds|qs)$")
  if (length(files) == 0) return(invisible())
  
  info <- file.info(files)
  now <- Sys.time()
  age <- difftime(now, info$mtime, units = "days")
  
  to_delete <- files[age > days_old]
  
  if (length(to_delete) > 0) {
    message(sprintf("Deleting %d old cache files...", length(to_delete)))
    unlink(to_delete)
  }
}

# -------------------------------------------------------------------------
# 4. Cache Inspection (List & Info)
# -------------------------------------------------------------------------

#' Retrieve metadata from a cached file
#' @export
cacheInfo <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  
  # Support QS if extension matches
  is_qs <- grepl("\\.qs$", path)
  
  obj <- if (is_qs) {
    if (!requireNamespace("qs", quietly=TRUE)) stop("qs package required")
    qs::qread(path)
  } else {
    readRDS(path)
  }
  
  # Check if it matches the new metadata structure
  is_new_format <- is.list(obj) && all(c("value", "meta") %in% names(obj))
  
  if (is_new_format) {
    return(obj)
  } else {
    # Legacy handling
    return(list(
      value = obj,
      meta = list(
        fname = NA_character_,
        created = file.info(path)$mtime,
        legacy = TRUE
      )
    ))
  }
}

#' List contents of a cache directory
#' @export
cacheList <- function(cache_dir = cacheR_default_dir()) {
  if (!dir.exists(cache_dir)) return(data.frame())
  
  files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.(rds|qs)$")
  if (length(files) == 0) return(data.frame())
  
  rows <- lapply(files, function(f) {
    # Lightweight check: mostly rely on file system stats
    # Loading every file to check metadata might be slow, 
    # so we might want to skip cacheInfo(f) for a simple list.
    # But if you need the function name, we must load.
    
    file_stat <- file.info(f)
    
    # Try to peek metadata (expensive)
    fname <- NA_character_
    try({
       # Only read if small enough? Or just accept the cost.
       info <- cacheInfo(f)
       if (!is.null(info$meta$fname)) fname <- info$meta$fname
    }, silent = TRUE)
    
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