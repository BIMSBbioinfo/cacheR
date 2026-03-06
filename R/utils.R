# --------------------------------------------- #
#' Retrieve Cached Value and Metadata
#'
#' Inspects a specific cache file to retrieve both the raw cached result and its 
#' associated metadata. This function is useful for debugging why a specific 
#' file was cached, checking the arguments used to generate it, or auditing 
#' creation times.
#'
#' @param file_path A character string specifying the path to the `.rds` or `.qs2`
#'   cache file.
#'
#' @return A list containing two elements:
#' \item{value}{The actual result returned by the cached function.}
#' \item{meta}{A list of metadata describing the cache entry. Fields include:
#'   \itemize{
#'     \item \code{fname}: The name of the function that was cached.
#'     \item \code{args}: A list of arguments passed to the function.
#'     \item \code{args_hash}: The hash generated from the arguments.
#'     \item \code{created}: A \code{POSIXct} timestamp of when the cache was created.
#'     \item \code{cache_file}: The full path to this cache file.
#'     \item \code{cache_dir}: The directory containing the cache.
#'   }
#' }
#' If the file is a legacy cache file (created before metadata was supported), 
#' the \code{meta} list will contain a \code{legacy = TRUE} flag and minimal information.
#'
#' @export
cacheInfo <- function(file_path) {
  if (!file.exists(file_path)) stop("Cache file not found: ", file_path)
  
  # Determine backend based on extension (simple heuristic)
  is_qs2 <- grepl("\\.qs2$", file_path, ignore.case = TRUE)

  obj <- if (is_qs2 && requireNamespace("qs2", quietly = TRUE)) {
    qs2::qs_read(file_path)
  } else {
    readRDS(file_path)
  }
  
  # 1. New Standard Format: list(value = ..., meta = ...)
  if (is.list(obj) && all(c("value", "meta") %in% names(obj))) {
    return(obj)
  }
  
  # 2. Legacy/Internal Format: list(dat = ..., meta = ...)
  # Some earlier versions might have used 'dat' instead of 'value'
  if (is.list(obj) && all(c("dat", "meta") %in% names(obj))) {
    return(list(value = obj$dat, meta = obj$meta))
  }
  
  # 3. Raw/Legacy Format (No metadata wrapper)
  # The file contains just the result object.
  list(
    value = obj,
    meta = list(
      fname      = NA_character_,
      cache_file = normalizePath(file_path, winslash = "/", mustWork = FALSE),
      created    = file.mtime(file_path),
      legacy     = TRUE
    )
  )
}

# --------------------------------------------- #
#' List and Summarize Cache Directory Contents
#'
#' Scans the specified cache directory and returns a summary of all valid cache files 
#' found. This provides a high-level view of the cache state, including which 
#' functions are cached, how much space they occupy, and when they were created.
#'
#' @param cache_dir A character string specifying the path to the cache directory.
#'
#' @return A \code{data.frame} where each row represents a cache file, containing 
#' the following columns:
#' \itemize{
#'   \item \code{file}: The filename of the cache entry.
#'   \item \code{fname}: The name of the cached function (if available in metadata).
#'   \item \code{created}: The creation timestamp.
#'   \item \code{size_bytes}: The size of the file in bytes.
#' }
#' Returns an empty data frame if the directory is empty or contains no valid cache files.
#'
#' @export
cacheList <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    warning("Cache directory does not exist: ", cache_dir)
    return(data.frame())
  }

  files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
  
  if (length(files) == 0) return(data.frame())
  
  # Helper to safely extract info without crashing on corrupt files
  get_meta_row <- function(f) {
    tryCatch({
      info <- cacheInfo(f)
      m <- info$meta
      
      data.frame(
        file       = basename(f),
        fname      = if (!is.null(m$fname)) m$fname else NA_character_,
        created    = if (!is.null(m$created)) m$created else file.mtime(f),
        size_bytes = file.size(f),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      # Fallback for unreadable files
      data.frame(
        file       = basename(f),
        fname      = NA_character_,
        created    = file.mtime(f),
        size_bytes = file.size(f),
        stringsAsFactors = FALSE
      )
    })
  }
  
  do.call(rbind, lapply(files, get_meta_row))
}

# --------------------------------------------- #
#' Aggregate Cache Statistics
#'
#' Computes summary statistics for a cache directory: number of entries,
#' total size, age range, and per-function breakdown.
#'
#' @param cache_dir A character string specifying the path to the cache directory.
#'
#' @return A list with:
#' \item{n_entries}{Integer count of cache files (excluding graph.rds).}
#' \item{total_size_mb}{Total size in megabytes (rounded to 2 decimal places).}
#' \item{oldest}{POSIXct timestamp of the oldest cache file.}
#' \item{newest}{POSIXct timestamp of the newest cache file.}
#' \item{by_function}{A data.frame with columns \code{fname}, \code{n_files},
#'   and \code{total_size_mb} summarizing entries per cached function.}
#'
#' @export
cache_stats <- function(cache_dir) {
  if (!dir.exists(cache_dir)) stop("Cache directory not found: ", cache_dir)

  files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
  # Exclude graph.rds
  files <- files[!grepl("^graph\\.rds$", basename(files))]

  if (length(files) == 0) {
    return(list(
      n_entries = 0L,
      total_size_mb = 0,
      oldest = as.POSIXct(NA),
      newest = as.POSIXct(NA),
      by_function = data.frame(
        fname = character(), n_files = integer(),
        total_size_mb = numeric(), stringsAsFactors = FALSE
      )
    ))
  }

  info <- file.info(files)
  sizes <- info$size
  mtimes <- info$mtime

  # Extract function name from metadata
  fnames <- vapply(files, function(f) {
    tryCatch({
      obj <- cacheInfo(f)
      m <- obj$meta
      if (!is.null(m$fname) && !is.na(m$fname)) m$fname else NA_character_
    }, error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)

  # Per-function breakdown
  if (any(!is.na(fnames))) {
    counts <- tapply(sizes, fnames, length)
    totals <- tapply(sizes, fnames, sum)
    by_func <- data.frame(
      fname = names(counts),
      n_files = as.integer(counts),
      total_size_mb = round(as.numeric(totals) / 1024^2, 2),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    by_func <- data.frame(
      fname = character(), n_files = integer(),
      total_size_mb = numeric(), stringsAsFactors = FALSE
    )
  }

  list(
    n_entries = length(files),
    total_size_mb = round(sum(sizes, na.rm = TRUE) / 1024^2, 2),
    oldest = min(mtimes, na.rm = TRUE),
    newest = max(mtimes, na.rm = TRUE),
    by_function = by_func
  )
}