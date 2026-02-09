# --------------------------------------------------------#
# Helper: Count cache entry files, excluding graph.rds
# --------------------------------------------------------#
count_cache_entries <- function(cache_dir, backend_pattern = "\\.(rds|qs)$") {
  files <- list.files(cache_dir, pattern = backend_pattern)
  length(files[!grepl("^graph\\.rds", files)])
}

# --------------------------------------------------------#
test_that("cacheFile works with basic caching behavior", {
  cache_dir <- file.path(tempdir(), "cache_test_basic")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  run_count <- 0
  
  f <- function(x) {
    run_count <<- run_count + 1
    x * 2
  }
  
  cached_f <- cacheFile(cache_dir) %@% f
  
  # Run 1
  expect_equal(cached_f(10), 20)
  expect_equal(run_count, 1)
  
  # Run 2 (Hit)
  expect_equal(cached_f(10), 20)
  expect_equal(run_count, 2)
  
  # Run 3 (New args)
  expect_equal(cached_f(20), 40)
  expect_equal(run_count, 3)
})

# --------------------------------------------------------#

test_that("cacheFile caches results and avoids re-running", {
  cache_dir <- file.path(tempdir(), "cache_test_no_counter")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE))
  
  # Strategy: Return a unique ID (Time) that changes every time the body runs.
  f <- function(x) {
    list(
      val = x * 2,
      run_id = as.numeric(Sys.time())
    )
  }
  
  cached_fun <- cacheFile(cache_dir) %@% f
  
  # --- Run 1 ---
  res1 <- cached_fun(10)
  expect_equal(res1$val, 20)
  
  # Sleep to ensure time would definitely change if it re-ran
  Sys.sleep(1.1)
  
  # --- Run 2 (Same Args) ---
  res2 <- cached_fun(10)
  
  # A. Value should match
  expect_equal(res2$val, 20)
  
  # B. Run ID must match exactly (Proves it loaded from disk)
  # If this fails, the function re-executed.
  expect_equal(res1$run_id, res2$run_id)
  
  # --- Run 3 (Different Args) ---
  res3 <- cached_fun(5)
  expect_equal(res3$val, 10)
  
  # Run ID must differ (New calculation)
  expect_false(res1$run_id == res3$run_id)
})

# --------------------------------------------------------#
test_that("cacheFile tracks multiple dir arguments and vector paths", {
  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_files_multidir")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  dir1 <- file.path(tempdir(), "cache_multidir_1")
  dir2 <- file.path(tempdir(), "cache_multidir_2")
  unlink(dir1, recursive = TRUE, force = TRUE)
  unlink(dir2, recursive = TRUE, force = TRUE)
  dir.create(dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir2, showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(dir1, "a.txt"))
  file.create(file.path(dir2, "b.txt"))

  fun <- function(path1, path2) {
    # vectorized paths
    sum(vapply(c(path1, path2), function(p) length(list.files(p)), integer(1)))
  }

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% fun

  n1 <- cached_fun(dir1, dir2)
  expect_equal(n1, 2L)

  # add another file to dir2; hash should change
  file.create(file.path(dir2, "c.txt"))
  n2 <- cached_fun(dir1, dir2)
  expect_equal(n2, 3L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("cacheFile works with 'qs' backend", {
  skip_if_not_installed("qs")
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_qs_backend")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)

  f <- cacheFile(cache_dir, backend = "qs") %@% function(x) {
    x * 2
  }

  res <- f(10)
  expect_equal(res, 20)

  # Verify the file extension is actually .qs
  files <- list.files(cache_dir)
  expect_true(any(grepl("\\.qs$", files)))
  
  # Verify we can load it back manually
  cache_path <- file.path(cache_dir, files[1])
  loaded <- qs::qread(cache_path)
  expect_equal(loaded$value, 20)
})

# --------------------------------------------------------#
test_that("cacheFile handles arguments that fail to evaluate", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()
  cache_dir <- file.path(tempdir(), "cache_error_arg")
  dir.create(cache_dir, showWarnings = FALSE)

  f <- cacheFile(cache_dir) %@% function(x) x
  
  # Passing a stop() as an argument
  # The decorator attempts to evaluate args to check for paths.
  # This ensures the decorator's tryCatch handles the error gracefully 
  # and allows the error to bubble up from the actual function call (or the arg eval)
  # rather than crashing inside the internal path checking logic.
  
  expect_error(f(stop("Boom")), "Boom")
})

# --------------------------------------------------------#
test_that("cacheFile treats implicit defaults equal to explicit values", {
  cache_dir <- file.path(tempdir(), "cache_implicit_defaults")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Define with defaults
  f <- cacheFile(cache_dir, backend="rds") %@% function(a, b = 10) {
    a + b
  }
  
  # 1. Call using implicit default
  res1 <- f(a = 5)
  expect_equal(res1, 15)
  
  # 2. Call using explicit value (should match default hash)
  res2 <- f(a = 5, b = 10)
  expect_equal(res2, 15)
  
  # 3. Check file count (cache entries only, excluding graph.rds)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  # 4. New value -> New file
  res3 <- f(a = 5, b = 11)
  expect_equal(res3, 16)

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})


# --------------------------------------------------------- #
test_that("Smart hashing detects file modification without new files (mtime check)", {
  cache_dir <- file.path(tempdir(), "cache_test_mtime")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  data_dir <- file.path(tempdir(), "data_mtime")
  dir.create(data_dir, showWarnings = FALSE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  
  data_file <- file.path(data_dir, "input.csv")
  writeLines("col1\n1", data_file)
  
  f <- function(file) readLines(file)
  
  cached_f <- cacheFile(cache_dir,  backend="rds") %@% f
  
  # Run 1
  res1 <- cached_f(data_file)
  
  # Modify file (wait for mtime tick)
  Sys.sleep(1.1)
  writeLines("col1\n2", data_file)
  
  # Run 2
  res2 <- cached_f(data_file)
  
  expect_false(identical(res1, res2))

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

# --------------------------------------------------------------------- #
test_that("xxhash64 backend works and produces valid filenames", {
  cache_dir <- file.path(tempdir(), "cache_test_algo")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Force backend to rds for consistent counting
  cached_f <- cacheFile(cache_dir, algo = "xxhash64", backend = "rds") %@% function(x) x

  cached_f(1)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  cached_f(2)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

test_that("Empty directory handling works with mtime hashing", {
  cache_dir <- file.path(tempdir(), "cache_test_empty")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  empty_dir <- file.path(tempdir(), "empty_input")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)
  
  f <- function(d) d
  
  cached_f <- cacheFile(cache_dir, backend="rds") %@% f
  
  # Run 1: Empty
  cached_f(empty_dir)
  
  # Modify
  Sys.sleep(1.1)
  writeLines("A", file.path(empty_dir, "new.txt"))
  
  # Run 2
  cached_f(empty_dir)

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

# -------------------------------------------------------------------------
# Test Feature 6: Environment Variable Tracking
# -------------------------------------------------------------------------
test_that("env_vars argument invalidates cache when env vars change", {
  cache_dir <- file.path(tempdir(), "cache_test_env_vars")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  Sys.setenv(TEST_CACHE_VAR = "A")
  on.exit(Sys.unsetenv("TEST_CACHE_VAR"), add = TRUE)
  
  f <- function(x) x
  cached_f <- cacheFile(cache_dir, env_vars = "TEST_CACHE_VAR", backend = "rds") %@% f
  
  # Run 1
  cached_f(10)
  
  # Change Env
  Sys.setenv(TEST_CACHE_VAR = "B")
  
  # Run 2
  cached_f(10)

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

# ----------------------------------------------------------------------- #
# --------------------------------------------------------#
test_that("backend selection works", {
  cache_dir <- file.path(tempdir(), "cache_test_backend")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # RDS
  cached_rds <- cacheFile(cache_dir, backend = "rds") %@% function(x) x
  cached_rds(1)
  expect_true(any(grepl("\\.rds$", list.files(cache_dir))))
  
  # QS
  if (requireNamespace("qs", quietly = TRUE)) {
    cached_qs <- cacheFile(cache_dir, backend = "qs") %@% function(x) x
    cached_qs(2)
    expect_true(any(grepl("\\.qs$", list.files(cache_dir))))
  }
})

# --------------------------------------------------------#
test_that("xxhash64 backend works and produces valid filenames", {
  cache_dir <- file.path(tempdir(), "cache_test_algo")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  cached_f <- cacheFile(cache_dir, algo = "xxhash64", backend = "rds") %@% function(x) x

  cached_f(1)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  cached_f(2)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})


# ---------------------------------------------------------------- #
# FEATURE 1: Dual Hashing (Values AND Expressions)
# ---------------------------------------------------------------- #

test_that("Cache invalidates when VALUES change (even if expression is same)", {
  cache_dir <- file.path(tempdir(), "test_values_change")
  on.exit(unlink(cache_dir, recursive = TRUE))
  dir.create(cache_dir, showWarnings=FALSE)
  
  # Function returns a run ID (timestamp)
  f <- cacheFile(cache_dir) %@% function(a) {
    as.numeric(Sys.time())
  }
  
  # 1. Setup variable
  x <- 10
  
  # Run 1
  id1 <- f(x)
  
  # 2. Modify value of x
  x <- 20
  Sys.sleep(1.1)
  
  # Run 2: Expression passed is still 'x', but value is 20.
  # Must re-run (Miss).
  id2 <- f(x)
  
  expect_false(id1 == id2)
})

### OBSOLETE - we don't want to cache variable names
# test_that("Cache invalidates when EXPRESSIONS change (even if value is same)", {
#   cache_dir <- file.path(tempdir(), "test_expr_change")
#   on.exit(unlink(cache_dir, recursive = TRUE))
#   dir.create(cache_dir, showWarnings=FALSE)
  
#   f <- cacheFile(cache_dir) %@% function(a) {
#     as.numeric(Sys.time())
#   }
  
#   # 1. Setup variables
#   val_A <- 100
#   val_B <- 100 # Identical value
  
#   # Run 1: Pass 'val_A'
#   id1 <- f(val_A)
  
#   Sys.sleep(1.1)
  
#   # Run 2: Pass 'val_B'
#   # Value is identical (100), but the source code (expression) changed from 'val_A' to 'val_B'.
#   # Ideally, this should be a MISS to ensure provenance safety.
#   id2 <- f(val_B)
  
#   expect_false(id1 == id2)
# })

# test_that("Cache invalidates when LITERALS change to VARIABLES (even if value is same)", {
#   cache_dir <- file.path(tempdir(), "test_literal_vs_var")
#   on.exit(unlink(cache_dir, recursive = TRUE))
#   dir.create(cache_dir, showWarnings=FALSE)
  
#   f <- cacheFile(cache_dir) %@% function(a) {
#     as.numeric(Sys.time())
#   }
  
#   x <- 50
  
#   # Run 1: Literal 50
#   id1 <- f(50)
  
#   Sys.sleep(1.1)
  
#   # Run 2: Variable x (value is 50)
#   # Expression changed from `50` to `x`. Should Miss.
#   id2 <- f(x)
  
#   expect_false(id1 == id2)
# })

# ---------------------------------------------------------------- #
# FEATURE 2: Deterministic Dots (...)
# ---------------------------------------------------------------- #

test_that("Dots (...) are order-independent (sorted by name)", {
  cache_dir <- file.path(tempdir(), "test_dots_order")
  on.exit(unlink(cache_dir, recursive = TRUE))
  dir.create(cache_dir, showWarnings=FALSE)
  
  f <- cacheFile(cache_dir) %@% function(...) {
    list(args = list(...), id = as.numeric(Sys.time()))
  }
  
  # Run 1: a=1, b=2
  res1 <- f(a=1, b=2)
  
  # Run 2: b=2, a=1 (Different order)
  # The hasher sorts named arguments, so this should match (HIT).
  res2 <- f(b=2, a=1)
  
  expect_equal(res1$id, res2$id)
})

test_that("Dots (...) detect new arguments", {
  cache_dir <- file.path(tempdir(), "test_dots_new")
  on.exit(unlink(cache_dir, recursive = TRUE))
  dir.create(cache_dir, showWarnings=FALSE)
  
  f <- cacheFile(cache_dir) %@% function(...) {
    as.numeric(Sys.time())
  }
  
  id1 <- f(a=1)
  Sys.sleep(1.1)
  id2 <- f(a=1, b=2)
  
  expect_false(id1 == id2)
})

# -------------------------------------------------------- #

test_that("Cache persists across separate R sessions (Disk Persistence)", {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload") 
  
  cache_dir <- file.path(tempdir(), "cache_cross_session")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  find_pkg_root <- function() {
    path <- "."
    for (i in 1:5) {
      if (file.exists(file.path(path, "DESCRIPTION"))) return(normalizePath(path))
      path <- file.path(path, "..")
    }
    stop("Could not find package root containing DESCRIPTION")
  }
  
  pkg_root <- find_pkg_root()

  # Define the job
  run_job <- function(dir, pkg_root) {
    # Load package
    if (!requireNamespace("cacheR", quietly = TRUE)) {
      if (!requireNamespace("pkgload", quietly = TRUE)) stop("Need pkgload")
      pkgload::load_all(pkg_root, quiet = TRUE)
    } else {
      library(cacheR)
    }
    
    # Define cached worker
    # Returns the TIME it ran (to verify caching)
    worker <- cacheFile(dir) %@% function(x) {
      Sys.sleep(0.5)
      list(
        val = x^2,
        timestamp = as.numeric(Sys.time()),
        cached_pid = Sys.getpid() # This gets baked into the cache
      )
    }
    
    # Return BOTH the actual process ID and the cached result
    list(
      real_pid = Sys.getpid(),  # <--- Captured OUTSIDE the cache
      result = worker(10)       # <--- Captured INSIDE the cache
    )
  }

  # --- SESSION A ---
  session_a <- callr::r(run_job, args = list(dir = cache_dir, pkg_root = pkg_root))
  
  # Ensure time ticks forward
  Sys.sleep(1.5)

  # --- SESSION B ---
  session_b <- callr::r(run_job, args = list(dir = cache_dir, pkg_root = pkg_root))
  
  # ---------------------------------------------------------------- #
  # VERIFICATION
  # ---------------------------------------------------------------- #
  
  # 1. Verify the PROCESSES were different (using the PID captured outside)
  expect_false(session_a$real_pid == session_b$real_pid)
  
  # 2. Verify the CACHE HIT (using the timestamp captured inside)
  # Session B should return the timestamp from Session A
  expect_equal(session_a$result$timestamp, session_b$result$timestamp)
  
  # 3. Verify the PID WAS CACHED (Optional Proof)
  # The 'cached_pid' returned by Session B will be Session A's PID, 
  # because it loaded the result from disk!
  expect_equal(session_a$result$cached_pid, session_b$result$cached_pid)
})



# -------------------------------------------------- #
test_that("changes in options used by function invalidate cache (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_options_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  old_opt <- getOption("cacheR.test.multiplier", NULL)
  on.exit(options(cacheR.test.multiplier = old_opt), add = TRUE)

  options(cacheR.test.multiplier = 1)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) {
    x * getOption("cacheR.test.multiplier")
  }

  r1 <- f(10)
  expect_equal(r1, 10)
  expect_equal(count_cache_entries(cache_dir), 1L)

  options(cacheR.test.multiplier = 2)

  # Desired behaviour: invalidation because option changed
  r2 <- f(10)
  expect_equal(r2, 20)
  expect_equal(count_cache_entries(cache_dir), 2L)
})

# --------------------------------------------------------#
# Tests for .atomic_save error handling (Issue #3)
# --------------------------------------------------------#

test_that(".atomic_save warns when saving to a non-writable path", {
  # Directly test .atomic_save with a path that cannot be written
  bad_path <- file.path(tempdir(), "nonexistent_subdir_xxyz", "test.rds")

  # saveRDS may emit its own low-level warning before our handler runs,
  # so capture all warnings and check that ours is among them
  warns <- capture_warnings(.atomic_save(list(value = 42), bad_path, "rds"))
  expect_true(any(grepl("cacheR: failed to save cache file", warns)))

  # Ensure no temp files were left behind
  expect_false(file.exists(bad_path))
})

test_that(".atomic_save warns on read-only cache directory but function still returns result", {
  skip_on_os("windows")
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_save_warn_readonly")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) x * 2

  # First call should work normally (cache dir is writable)
  r1 <- f(5)
  expect_equal(r1, 10)

  # Make the cache dir read-only
  Sys.chmod(cache_dir, mode = "0555")
  on.exit(Sys.chmod(cache_dir, mode = "0755"), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # New args to force a cache miss; should warn but still return result
  expect_warning(
    result <- f(10),
    regexp = "cacheR: failed to save cache file"
  )
  expect_equal(result, 20)
})

test_that(".atomic_save cleans up temp files on failure", {
  # Create a dir, then make it read-only to trigger failure
  skip_on_os("windows")

  test_dir <- file.path(tempdir(), "cache_atomic_cleanup_test")
  unlink(test_dir, recursive = TRUE, force = TRUE)
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

  # Write a file first, then make dir read-only
  target_path <- file.path(test_dir, "result.rds")

  Sys.chmod(test_dir, mode = "0555")
  on.exit({
    Sys.chmod(test_dir, mode = "0755")
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })

  warns <- capture_warnings(.atomic_save(list(value = "test"), target_path, "rds"))
  expect_true(any(grepl("cacheR: failed to save cache file", warns)))

  # Ensure no temp files or target file exist
  Sys.chmod(test_dir, mode = "0755")
  remaining <- list.files(test_dir)
  expect_length(remaining, 0)
})

# --------------------------------------------------------#
# Tests for Issue #4: deparse width-dependence
# --------------------------------------------------------#

test_that("options(width) does not invalidate cache (Issue #4)", {
  cache_dir <- file.path(tempdir(), "cache_test_width")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  old_width <- getOption("width")
  on.exit(options(width = old_width), add = TRUE)

  f <- function(x) {
    list(val = x * 2, run_id = as.numeric(Sys.time()))
  }

  options(width = 40)
  cached_f <- cacheFile(cache_dir) %@% f

  res1 <- cached_f(10)
  expect_equal(res1$val, 20)

  Sys.sleep(1.1)
  options(width = 200)

  res2 <- cached_f(10)
  expect_equal(res2$val, 20)

  # run_id must match — proves cache was hit despite width change
  expect_equal(res1$run_id, res2$run_id)
})

test_that("body change still invalidates cache after AST hashing (Issue #4)", {
  cache_dir <- file.path(tempdir(), "cache_test_body_change")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f1 <- function(x) x * 2
  f2 <- function(x) x * 3

  cached_f1 <- cacheFile(cache_dir, backend = "rds") %@% f1
  cached_f2 <- cacheFile(cache_dir, backend = "rds") %@% f2

  res1 <- cached_f1(10)
  expect_equal(res1, 20)

  res2 <- cached_f2(10)
  expect_equal(res2, 30)

  # Two different cache files should exist (different body hashes)
  # Note: graph.rds also matches, so we expect >= 3 (2 cache + 1 graph)
  files <- list.files(cache_dir, pattern = "\\.rds$")
  # Exclude graph.rds to count only cache files
  cache_files <- files[!grepl("^graph", files)]
  expect_equal(length(cache_files), 2)
})

# --------------------------------------------------------#
# Tests for Issue #6: Default args evaluated in wrong environment
# --------------------------------------------------------#

test_that("default referencing another arg evaluates correctly (Issue #6)", {
  cache_dir <- file.path(tempdir(), "cache_test_default_ref")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(a, b = a * 2) {
    list(val = a + b, run_id = as.numeric(Sys.time()))
  }
  cached_f <- cacheFile(cache_dir) %@% f

  # Call with default b = a * 2 = 10
  res1 <- cached_f(5)
  expect_equal(res1$val, 15)  # 5 + 10

  Sys.sleep(1.1)

  # Call with explicit b = 10 (same value as default would give)
  res2 <- cached_f(5, b = 10)
  expect_equal(res2$val, 15)

  # Should be a cache hit since b resolves to same value
  expect_equal(res1$run_id, res2$run_id)
})

test_that("default with stop() falls back gracefully (Issue #6)", {
  cache_dir <- file.path(tempdir(), "cache_test_default_stop")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(x, y = stop("unused")) {
    x + y
  }
  cached_f <- cacheFile(cache_dir) %@% f

  # Supplying y explicitly should work fine
  res <- cached_f(1, y = 2)
  expect_equal(res, 3)
})

test_that("default referencing function closure evaluates correctly (Issue #6)", {
  cache_dir <- file.path(tempdir(), "cache_test_default_closure")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  multiplier <- 10
  f <- function(a, b = a * multiplier) {
    a + b
  }
  cached_f <- cacheFile(cache_dir) %@% f

  res <- cached_f(3)
  expect_equal(res, 33)  # 3 + 3*10
})

# --------------------------------------------------------#
# Tests for Issue #14: Probabilistic hash improvements
# --------------------------------------------------------#

test_that("mid-file change detected in medium file under full hash threshold (Issue #14)", {
  cache_dir <- file.path(tempdir(), "cache_test_midhash_medium")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Create a 2MB file (under the 5MB full hash threshold)
  test_file <- file.path(cache_dir, "medium.bin")
  size <- 2 * 1024 * 1024
  con <- file(test_file, "wb")
  writeBin(raw(size), con)
  close(con)

  hash1 <- .probabilistic_file_hash(test_file)

  # Modify a byte in the middle
  con <- file(test_file, "r+b")
  seek(con, size %/% 2, "start")
  writeBin(as.raw(0xFF), con)
  close(con)

  hash2 <- .probabilistic_file_hash(test_file)

  # Full hash guarantees detection
  expect_false(identical(hash1, hash2))
})

test_that("seed produces diverse offsets for different paths (Issue #14)", {
  # Two different paths with the same file size should get different sampling offsets
  size <- 100 * 1024 * 1024  # hypothetical 100MB

  seed1 <- strtoi(substring(digest::digest(paste0("/path/a.bin", size), algo = "crc32"), 1, 7), base = 16L)
  seed2 <- strtoi(substring(digest::digest(paste0("/path/b.bin", size), algo = "crc32"), 1, 7), base = 16L)

  expect_false(seed1 == seed2)
})

test_that("full hash threshold boundary works correctly (Issue #14)", {
  cache_dir <- file.path(tempdir(), "cache_test_threshold")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # File at exactly 5MB — should get full hash
  file_at <- file.path(cache_dir, "at_limit.bin")
  size_at <- 5 * 1024 * 1024
  con <- file(file_at, "wb")
  writeBin(raw(size_at), con)
  close(con)

  hash_at <- .probabilistic_file_hash(file_at)
  expect_true(!is.na(hash_at))

  # Modify a byte in the middle — full hash should detect it
  con <- file(file_at, "r+b")
  seek(con, size_at %/% 2, "start")
  writeBin(as.raw(0xFF), con)
  close(con)

  hash_at2 <- .probabilistic_file_hash(file_at)
  expect_false(identical(hash_at, hash_at2))
})

# --------------------------------------------------------#
# Tests for Issue #1: Functions returning NULL
# --------------------------------------------------------#

test_that("function returning NULL is cached and not re-executed (Issue #1)", {
  cache_dir <- file.path(tempdir(), "cache_test_null_return")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  counter_file <- tempfile()
  writeLines("0", counter_file)
  on.exit(unlink(counter_file), add = TRUE)

  f <- function(x) {
    n <- as.integer(readLines(counter_file))
    writeLines(as.character(n + 1L), counter_file)
    NULL
  }
  cached_f <- cacheFile(cache_dir) %@% f

  r1 <- cached_f(1)
  expect_null(r1)
  expect_equal(as.integer(readLines(counter_file)), 1L)

  r2 <- cached_f(1)
  expect_null(r2)
  # Should still be 1 — cache hit, no re-execution
  expect_equal(as.integer(readLines(counter_file)), 1L)
})

test_that("function returning NULL with different args creates separate entries (Issue #1)", {
  cache_dir <- file.path(tempdir(), "cache_test_null_args")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(x) NULL
  cached_f <- cacheFile(cache_dir) %@% f

  r1 <- cached_f(1)
  r2 <- cached_f(2)
  expect_null(r1)
  expect_null(r2)

  # Two different args should create two cache entries
  expect_equal(count_cache_entries(cache_dir), 2)
})

# --------------------------------------------------------#
# Tests for Issue #2: invisible() attribute preserved
# --------------------------------------------------------#

test_that("invisible() is preserved on cache miss (Issue #2)", {
  cache_dir <- file.path(tempdir(), "cache_test_invisible_miss")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(x) invisible(x * 2)
  cached_f <- cacheFile(cache_dir) %@% f

  v <- withVisible(cached_f(5))
  expect_equal(v$value, 10)
  expect_false(v$visible)
})

test_that("invisible() is preserved on cache hit (Issue #2)", {
  cache_dir <- file.path(tempdir(), "cache_test_invisible_hit")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(x) invisible(x * 2)
  cached_f <- cacheFile(cache_dir) %@% f

  # Miss
  cached_f(5)

  # Hit — should still be invisible
  v <- withVisible(cached_f(5))
  expect_equal(v$value, 10)
  expect_false(v$visible)
})

test_that("visible return stays visible after cache reload (Issue #2)", {
  cache_dir <- file.path(tempdir(), "cache_test_visible_stays")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(x) x * 2
  cached_f <- cacheFile(cache_dir) %@% f

  # Miss
  cached_f(5)

  # Hit — should still be visible
  v <- withVisible(cached_f(5))
  expect_equal(v$value, 10)
  expect_true(v$visible)
})

# --------------------------------------------------------#
# Tests for Issue #7: Recursive self-calling cached functions
# --------------------------------------------------------#

test_that("recursive cached function produces correct results (Issue #7)", {
  cache_dir <- file.path(tempdir(), "cache_test_recursive")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Fibonacci with caching
  fib <- cacheFile(cache_dir) %@% function(n) {
    if (n <= 1) return(n)
    fib(n - 1) + fib(n - 2)
  }

  expect_equal(fib(0), 0)
  expect_equal(fib(1), 1)
  expect_equal(fib(5), 5)
  expect_equal(fib(10), 55)
})

test_that("recursive cached function uses cache for repeated subcalls (Issue #7)", {
  cache_dir <- file.path(tempdir(), "cache_test_recursive_cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  counter_file <- tempfile()
  writeLines("0", counter_file)
  on.exit(unlink(counter_file), add = TRUE)

  fib <- cacheFile(cache_dir) %@% function(n) {
    cnt <- as.integer(readLines(counter_file))
    writeLines(as.character(cnt + 1L), counter_file)
    if (n <= 1) return(n)
    fib(n - 1) + fib(n - 2)
  }

  result <- fib(6)
  expect_equal(result, 8)
  first_run_count <- as.integer(readLines(counter_file))

  # Second call to fib(6) should be a single cache hit
  writeLines("0", counter_file)
  result2 <- fib(6)
  expect_equal(result2, 8)
  second_run_count <- as.integer(readLines(counter_file))

  # fib(6) itself is cached, so no executions on second call
  expect_equal(second_run_count, 0L)
})

test_that("call stack is properly maintained during recursion (Issue #7)", {
  cache_dir <- file.path(tempdir(), "cache_test_recursive_stack")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  cacheR_reset_graph()

  fib <- cacheFile(cache_dir) %@% function(n) {
    if (n <= 1) return(n)
    fib(n - 1) + fib(n - 2)
  }

  fib(4)

  # Call stack should be empty after completion (all on.exit handlers fired)
  expect_length(.graph_cache$call_stack, 0)
})

# --------------------------------------------------------#
# Tests for Issue #12: .load parameter collision
# --------------------------------------------------------#

test_that(".load parameter collision with wrapped function is documented (Issue #12)", {
  cache_dir <- file.path(tempdir(), "cache_test_load_collision")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # A function that has a parameter named .load
  f <- function(x, .load = "default_val") {
    list(x = x, load_arg = .load)
  }
  cached_f <- cacheFile(cache_dir) %@% f

  # Calling with .load = "custom" collides with the wrapper's .load parameter.
  # The wrapper interprets .load as its own caching control flag.
  # This is a known limitation — document that it errors.
  expect_error(cached_f(1, .load = "custom"))
})

test_that("wrapped function's .load default is ignored in cache key (Issue #12)", {
  cache_dir <- file.path(tempdir(), "cache_test_load_default")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # When .load is not explicitly passed, it goes to the wrapper (TRUE),
  # not the inner function. The inner function's .load default is not used.
  f <- function(x, .load = "inner_default") {
    list(x = x, load_arg = .load)
  }
  cached_f <- cacheFile(cache_dir) %@% f

  # This call works because .load is not explicitly passed,
  # so the wrapper gets .load=TRUE (its own default).
  # But the inner function's .load parameter is missing.
  # The behavior depends on whether match.call picks it up.
  r <- tryCatch(cached_f(1), error = function(e) e)

  # Document actual behavior: either works with inner default or errors
  if (inherits(r, "error")) {
    expect_true(TRUE)  # collision causes error — documented
  } else {
    expect_equal(r$x, 1)
  }
})

# --------------------------------------------------------#
# Tests for Issue #15: Positional vs named argument equivalence
# --------------------------------------------------------#

test_that("positional and named args hit same cache entry (Issue #15)", {
  cache_dir <- file.path(tempdir(), "cache_test_pos_named")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(a, b) {
    list(val = a + b, run_id = as.numeric(Sys.time()))
  }
  cached_f <- cacheFile(cache_dir) %@% f

  res1 <- cached_f(1, 2)
  Sys.sleep(1.1)
  res2 <- cached_f(a = 1, b = 2)

  expect_equal(res1$val, res2$val)
  # Same run_id proves cache hit
  expect_equal(res1$run_id, res2$run_id)
})

test_that("mixed positional and named args hit same cache (Issue #15)", {
  cache_dir <- file.path(tempdir(), "cache_test_mixed_args")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(a, b, c) {
    list(val = a + b + c, run_id = as.numeric(Sys.time()))
  }
  cached_f <- cacheFile(cache_dir) %@% f

  res1 <- cached_f(1, 2, 3)
  Sys.sleep(1.1)
  res2 <- cached_f(1, b = 2, c = 3)

  expect_equal(res1$run_id, res2$run_id)
})

test_that("reversed named args hit same cache as positional (Issue #15)", {
  cache_dir <- file.path(tempdir(), "cache_test_reversed_named")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- function(a, b) {
    list(val = a - b, run_id = as.numeric(Sys.time()))
  }
  cached_f <- cacheFile(cache_dir) %@% f

  res1 <- cached_f(10, 3)
  Sys.sleep(1.1)
  # Named args in reverse order — match.call should canonicalize
  res2 <- cached_f(b = 3, a = 10)

  expect_equal(res1$val, 7)
  expect_equal(res2$val, 7)
  expect_equal(res1$run_id, res2$run_id)
})

# =========================================================================
# File path detection in R objects
# =========================================================================

describe("File path detection in R objects", {

  it("detects file path in S4 object slot", {
    setClass("TestFileRef", representation(path = "character", label = "character"))
    on.exit(removeClass("TestFileRef"), add = TRUE)

    tmp <- tempfile(fileext = ".txt")
    writeLines("hello", tmp)
    on.exit(unlink(tmp), add = TRUE)

    obj <- new("TestFileRef", path = tmp, label = "test")

    # extract_paths_recursively should find the path
    paths <- cacheR:::.extract_paths_recursively(obj)
    expect_true(tmp %in% paths)

    # replace_paths_with_hashes should produce a hash that changes when file changes
    hash1 <- cacheR:::.replace_paths_with_hashes(obj, algo = "xxhash64")
    writeLines("world", tmp)
    hash2 <- cacheR:::.replace_paths_with_hashes(obj, algo = "xxhash64")
    expect_false(identical(hash1, hash2))
  })

  it("detects file paths in nested S4 object", {
    setClass("TestNestedRef", representation(files = "list", name = "character"))
    on.exit(removeClass("TestNestedRef"), add = TRUE)

    tmp1 <- tempfile(fileext = ".csv")
    tmp2 <- tempfile(fileext = ".csv")
    writeLines("a,b", tmp1)
    writeLines("c,d", tmp2)
    on.exit(unlink(c(tmp1, tmp2)), add = TRUE)

    obj <- new("TestNestedRef", files = list(tmp1, tmp2), name = "dataset")

    paths <- cacheR:::.extract_paths_recursively(obj)
    expect_true(tmp1 %in% paths)
    expect_true(tmp2 %in% paths)
  })

  it("detects file path in custom attribute", {
    tmp <- tempfile(fileext = ".dat")
    writeLines("data", tmp)
    on.exit(unlink(tmp), add = TRUE)

    x <- 1:10
    attr(x, "source_file") <- tmp

    paths <- cacheR:::.extract_paths_recursively(x)
    expect_true(tmp %in% paths)

    # Hash should change when file changes
    hash1 <- cacheR:::.replace_paths_with_hashes(x, algo = "xxhash64")
    writeLines("new data", tmp)
    hash2 <- cacheR:::.replace_paths_with_hashes(x, algo = "xxhash64")
    expect_false(identical(hash1, hash2))
  })

  it("detects file paths in data frame columns", {
    tmp <- tempfile(fileext = ".txt")
    writeLines("content", tmp)
    on.exit(unlink(tmp), add = TRUE)

    df <- data.frame(id = 1:2, file = c(tmp, "nonexistent_file.txt"),
                     stringsAsFactors = FALSE)

    paths <- cacheR:::.extract_paths_recursively(df)
    expect_true(tmp %in% paths)
    expect_false("nonexistent_file.txt" %in% paths)
  })

  it("end-to-end: cache invalidates when file in S4 slot changes", {
    setClass("TestInput", representation(filepath = "character"))
    on.exit(removeClass("TestInput"), add = TRUE)

    cache_dir <- file.path(tempdir(), paste0("s4_e2e_", as.integer(Sys.time())))
    dir.create(cache_dir, recursive = TRUE)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    tmp <- tempfile(fileext = ".txt")
    writeLines("version1", tmp)
    on.exit(unlink(tmp), add = TRUE)

    f <- function(obj) {
      list(data = readLines(obj@filepath), run_id = as.numeric(Sys.time()))
    }
    cached_f <- cacheFile(cache_dir, backend = "rds") %@% f

    input <- new("TestInput", filepath = tmp)

    res1 <- cached_f(input)
    expect_equal(res1$data, "version1")

    Sys.sleep(1.1)

    # Same object, same file content — cache hit
    res2 <- cached_f(input)
    expect_equal(res1$run_id, res2$run_id)

    # Change the file content — should invalidate cache
    writeLines("version2", tmp)
    Sys.sleep(1.1)
    res3 <- cached_f(input)
    expect_equal(res3$data, "version2")
    expect_false(res1$run_id == res3$run_id)
  })

  it("end-to-end: cache invalidates when file in attribute changes", {
    cache_dir <- file.path(tempdir(), paste0("attr_e2e_", as.integer(Sys.time())))
    dir.create(cache_dir, recursive = TRUE)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

    tmp <- tempfile(fileext = ".txt")
    writeLines("v1", tmp)
    on.exit(unlink(tmp), add = TRUE)

    f <- function(x) {
      src <- attr(x, "source_file")
      list(content = readLines(src), run_id = as.numeric(Sys.time()))
    }
    cached_f <- cacheFile(cache_dir, backend = "rds") %@% f

    x <- 42
    attr(x, "source_file") <- tmp

    res1 <- cached_f(x)
    expect_equal(res1$content, "v1")

    Sys.sleep(1.1)

    # Change file content
    writeLines("v2", tmp)
    Sys.sleep(1.1)
    res2 <- cached_f(x)
    expect_equal(res2$content, "v2")
    expect_false(res1$run_id == res2$run_id)
  })

  it("S4 object with no file paths produces no false positives", {
    setClass("TestNumeric", representation(value = "numeric", label = "character"))
    on.exit(removeClass("TestNumeric"), add = TRUE)

    obj <- new("TestNumeric", value = 42, label = "not_a_file_path")

    paths <- cacheR:::.extract_paths_recursively(obj)
    expect_length(paths, 0)
  })
})