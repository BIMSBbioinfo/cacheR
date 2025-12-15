library(testthat)

# --------------------------------------------- #
test_that("cacheFile stores value and metadata in cache file", {
  cache_dir <- file.path(tempdir(), "cache_meta_basic")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- function(x, y = 1) {
    x + y
  }

  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL, backend="rds") %@% f

  # First call – should compute and write cache
  expect_equal(cached(10), 11)
  
  # FIX: Filter for .rds or .qs files only, ignoring .lock files
  files <- list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE)
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
  expect_equal(meta$args$x, 10)
})

# --------------------------------------------- #
test_that("cached function still returns raw value despite metadata wrapper", {
  cache_dir <- file.path(tempdir(), "cache_meta_value_only")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # STRATEGY: Return a unique ID (Timestamp) to prove caching
  # If the cache works, the returned timestamp will be STALE (from the first run).
  f <- function(x) {
    list(
      result = x * 2,
      run_id = as.numeric(Sys.time())
    )
  }

  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL) %@% f

  # --- Run 1 ---
  res1 <- cached(5)
  expect_equal(res1$result, 10)

  # Wait to ensure time would change if it re-ran
  Sys.sleep(1.1)

  # --- Run 2 ---
  res2 <- cached(5)
  
  # 1. Value check (User transparency)
  expect_equal(res2$result, 10)
  
  # 2. Cache Hit check (No Counter needed)
  # The run_id must match the first run. If it re-ran, this would fail.
  expect_equal(res1$run_id, res2$run_id)
})

# --------------------------------------------- #

test_that("cacheInfo returns value and metadata", {
  cache_dir <- file.path(tempdir(), "cache_meta_info")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- function(x, y = 2) x * y
  cached <- cacheFile(cache_dir = cache_dir, ignore_args = NULL, backend="rds") %@% f

  # Generate cache
  cached(3)

  files <- list.files(cache_dir, full.names = TRUE, pattern="\\.(rds|qs)$")
  expect_length(files, 1L)

  # Test cacheInfo
  info <- cacheInfo(files[1])
  expect_true(is.list(info))
  expect_true(all(c("value", "meta") %in% names(info)))
  expect_identical(info$value, 6)

  expect_equal(info$meta$args$x, 3)
})

# --------------------------------------------- #
test_that("cacheList summarizes cache directory contents", {
  cache_dir <- file.path(tempdir(), "cache_meta_list")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
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
})

# --------------------------------------------- #
test_that("cacheInfo gracefully handles legacy cache files without metadata", {
  cache_dir <- file.path(tempdir(), "cache_meta_legacy")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  legacy_path <- file.path(cache_dir, "legacy.rds")
  # Manually save a "dumb" value (not wrapped in list(value=, meta=))
  saveRDS(123L, legacy_path)

  info <- cacheInfo(legacy_path)

  # Should normalize it to the new structure
  expect_true(is.list(info))
  expect_identical(info$value, 123L)
  expect_true(is.list(info$meta))
  expect_true(info$meta$legacy)
})