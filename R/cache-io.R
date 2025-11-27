# ----------------------------------------------------- #
.cacheR_save <- function(object, path, backend = c("rds", "qs")) {
  backend <- match.arg(backend)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (backend == "rds") {
    saveRDS(object, path)
  } else {  # "qs"
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop(
        "Backend 'qs' requires the 'qs' package. ",
        "Install it with install.packages('qs') or use backend = 'rds'.",
        call. = FALSE
      )
    }
    qs::qsave(object, path)
  }

  invisible(path)
}


# ----------------------------------------------------- #
.cacheR_load <- function(path, backend = NULL) {
  if (!file.exists(path)) {
    stop("Cache file does not exist: ", path, call. = FALSE)
  }

  # If backend not given, infer from file extension (.rds / .qs)
  if (is.null(backend)) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("rds", "qs")) {
      backend <- ext
    } else {
      backend <- "rds"  # sensible default / fallback
    }
  }

  backend <- match.arg(backend, c("rds", "qs"))

  if (backend == "rds") {
    readRDS(path)
  } else {  # "qs"
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop(
        "Backend 'qs' requires the 'qs' package. ",
        "Install it with install.packages('qs') or use backend = 'rds'.",
        call. = FALSE
      )
    }
    qs::qread(path)
  }
}


