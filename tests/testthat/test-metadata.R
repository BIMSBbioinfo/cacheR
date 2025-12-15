# ---------------------------------------------------------------- #
test_that("cacheFile stores value and metadata in cache file", {
  cache_dir <- file.path(tempdir(), "cache_meta_basic")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- function(x, y = 1) {
    x + y
  }

  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% f

  # First call – should compute and write cache
  expect_equal(cached(10), 11)
  expect_equal(runs, 1L)

  files <- list.files(cache_dir, full.names = TRUE)
  expect_length(files, 1L)

  obj <- readRDS(files[1])

  # New structure: list(value = ..., meta = list(...))
  expect_true(is.list(obj))
  expect_true(all(c("value", "meta") %in% names(obj)))

  expect_identical(obj$value, 11)

  meta <- obj$meta
  expect_true(is.list(meta))

  # Basic fields we expect to be present
  expect_true(all(c("fname", "args", "args_hash", "cache_file", "cache_dir", "created") %in% names(meta)))

  expect_equal(meta$fname, "f")
  expect_true(is.list(meta$args))
  expect_equal(meta$args$x, 10)
  expect_equal(meta$args$y, 1)

  expect_true(is.character(meta$args_hash))
  expect_true(nchar(meta$args_hash) > 0)

  expect_identical(meta$cache_dir, normalizePath(cache_dir, winslash = "/", mustWork = TRUE))
  expect_identical(meta$cache_file, normalizePath(files[1], winslash = "/", mustWork = TRUE))
  expect_true(inherits(meta$created, "POSIXct"))
})

test_that("cached function still returns raw value despite metadata wrapper", {
  cache_dir <- file.path(tempdir(), "cache_meta_value_only")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  runs <- 0L
  f <- function(x) {
    runs <<- runs + 1L
    x * 2
  }

  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% f

  # First call – MISS
  expect_equal(cached(5), 10)
  expect_equal(runs, 1L)

  # Second call – HIT, but still returns plain 10
  expect_equal(cached(5), 10)
  expect_equal(runs, 1L)
})

# ---------------------------------------------------------------- #
test_that("cacheInfo returns value and metadata", {
  cache_dir <- file.path(tempdir(), "cache_meta_info")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- function(x, y = 2) x * y
  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% f

  expect_equal(cached(3), 6)

  files <- list.files(cache_dir, full.names = TRUE)
  expect_length(files, 1L)

  info <- cacheInfo(files[1])
  expect_true(is.list(info))
  expect_true(all(c("value", "meta") %in% names(info)))

  expect_identical(info$value, 6)

  meta <- info$meta
  expect_true(is.list(meta))
  expect_equal(meta$fname, "f")
  expect_true(is.list(meta$args))
  expect_equal(meta$args$x, 3)
  expect_equal(meta$args$y, 2)
})

test_that("cacheList summarizes cache directory contents", {
  cache_dir <- file.path(tempdir(), "cache_meta_list")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- function(x) x + 1
  g <- function(z) z * 10

  cf <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% f
  cg <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% g

  cf(1)
  cg(2)

  df <- cacheList(cache_dir)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)

  expect_true(all(c("file", "fname", "created", "size_bytes") %in% names(df)))
  expect_true(all(df$size_bytes > 0))

  # Function names should be present
  expect_true(all(c("f", "g") %in% df$fname))
})

# ---------------------------------------------------------------- #
test_that("cacheInfo gracefully handles legacy cache files without metadata", {
  cache_dir <- file.path(tempdir(), "cache_meta_legacy")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  legacy_path <- file.path(cache_dir, "legacy.rds")
  saveRDS(123L, legacy_path)

  info <- cacheInfo(legacy_path)

  expect_true(is.list(info))
  expect_true(all(c("value", "meta") %in% names(info)))
  expect_identical(info$value, 123L)

  # meta will be minimal but present
  expect_true(is.list(info$meta))
  expect_identical(info$meta$cache_file, normalizePath(legacy_path, winslash = "/", mustWork = TRUE))
})