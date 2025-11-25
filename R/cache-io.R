.cacheR_save <- function(object, path, backend = "rds") {
  backend <- match.arg(backend, c("rds", "qs"))

  if (backend == "rds") {
    saveRDS(object, path)
    return(invisible(path))
  }

  # backend == "qs"
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Backend 'qs' requested, but the 'qs' package is not installed.\n",
         "Install it via install.packages('qs') or use backend = 'rds'.",
         call. = FALSE)
  }

  qs::qsave(object, path, preset = "fast")  # or "high" if you prefer
  invisible(path)
}

.cacheR_load <- function(path, backend = "rds") {
  backend <- match.arg(backend, c("rds", "qs"))

  if (!file.exists(path)) {
    stop("Cache file does not exist: ", path, call. = FALSE)
  }

  if (backend == "rds") {
    return(readRDS(path))
  }

  # backend == "qs"
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Backend 'qs' requested, but the 'qs' package is not installed.\n",
         "Install it via install.packages('qs') or use backend = 'rds'.",
         call. = FALSE)
  }

  qs::qread(path)
}
