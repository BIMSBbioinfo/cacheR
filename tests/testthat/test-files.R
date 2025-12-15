library(testthat)

# --------------------------------------------------------#
# Helper: Reset cache tree if available
# --------------------------------------------------------#
reset_if_needed <- function() {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }
}

# --------------------------------------------------------#
test_that("Automatic Detection: invalidates when number of files in arg path changes", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_test_files_arg")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_dir_arg")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  # start with one file
  file.create(file.path(input_dir, "file1.txt"))

  # NO file_args needed anymore
  cached_fun <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    length(list.files(path))
  }

  n1 <- cached_fun(input_dir)
  expect_equal(n1, 1L)

  # add a second file; should force a new cache key automatically
  Sys.sleep(1.1) # Ensure mtime update
  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(input_dir)
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("Automatic Detection: does not invalidate when file counts stay the same", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_test_files_stable")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_dir_stable")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(input_dir, "a.txt"))
  file.create(file.path(input_dir, "b.txt"))

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    files <- sort(list.files(path))
    paste(files, collapse = ",")
  }

  res1 <- cached_fun(input_dir)
  res2 <- cached_fun(input_dir)  # should hit the cache

  expect_identical(res1, res2)
})

# --------------------------------------------------------#
test_that("Automatic Detection: works with base::list.files and explicit namespaces", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_files_base_listfiles")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_base_listfiles")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(input_dir, "file1.txt"))

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    length(base::list.files(path))
  }

  n1 <- cached_fun(input_dir)
  expect_equal(n1, 1L)

  Sys.sleep(1.1)
  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(input_dir)
  expect_equal(n2, 2L)
})

# --------------------------------------------------------#
test_that("Automatic Detection: works when path is passed via ... (dots)", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_test_files_dots")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_dir_dots")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  file.create(file.path(input_dir, "file1.txt"))

  fun <- function(...) {
    args <- list(...)
    path <- args$path
    length(list.files(path))
  }

  # The new logic scans arguments in '...' automatically
  cached_fun <- cacheFile(cache_dir = cache_dir) %@% fun

  n1 <- cached_fun(path = input_dir)
  expect_equal(n1, 1L)

  Sys.sleep(1.1)
  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(path = input_dir)
  expect_equal(n2, 2L)
})

# --------------------------------------------------------#
test_that("Normalization: Relative and Absolute paths hit same logic", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()
  
  cache_dir <- file.path(tempdir(), "cache_test_norm")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  data_dir <- file.path(tempdir(), "data_subdir")
  dir.create(data_dir, showWarnings = FALSE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(data_dir, "test.csv")
  writeLines("A,B\n1,2", file_path)
  
  f <- cacheFile(cache_dir) %@% function(path) readLines(path)
  
  # 1. Call with Absolute Path
  f(file_path)
  
  # 2. Call with Relative Path
  old_wd <- getwd()
  setwd(data_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  # Even though the string is different ("test.csv" vs "/tmp/.../test.csv"),
  # they resolve to the same file. The cache key includes the file hash.
  # So while the 'args' part of the hash differs, the 'dir_states' part is identical.
  # This creates a SAFE new cache entry.
  
  res <- f("test.csv")
  expect_equal(res, c("A,B", "1,2"))
})

# --------------------------------------------------------#
test_that("Multiple Arguments: Scans all arguments for files", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()
  
  cache_dir <- file.path(tempdir(), "cache_test_scan_args")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  dir1 <- file.path(tempdir(), "dir1")
  dir2 <- file.path(tempdir(), "dir2")
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  on.exit(unlink(c(dir1, dir2), recursive = TRUE), add = TRUE)
  
  # Function taking two paths
  f <- function(a, b) {
    paste(a, b)
  }
  
  cached_f <- cacheFile(cache_dir, backend = "rds") %@% f
  
  # Run 1
  cached_f(dir1, "some_val")
  
  # Run 2: Different directory arg
  cached_f(dir2, "some_val")
  
  # Should have 2 cache files
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
})

# --------------------------------------------------------#
test_that("Safety: Non-file strings are ignored (no crash)", {
  getOption("cacheR.backend", "rds")

  reset_if_needed()
  
  cache_dir <- file.path(tempdir(), "cache_test_safety")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  f <- cacheFile(cache_dir) %@% function(mode, label) {
    paste(mode, label)
  }
  
  # "fast" and "run1" do not exist on disk.
  # The hasher should check file.exists("fast") -> FALSE -> Ignore.
  expect_error(f("fast", "run1"), NA)
  expect_equal(f("fast", "run1"), "fast run1")
})

# --------------------------------------------------------#
test_that("Edge Case: The 'Coincidence' False Positive", {
  getOption("cacheR.backend", "rds")

  # This test confirms the 'robustness' trade-off:
  # If an argument name matches a file on disk by pure luck, 
  # changing that file invalidates the cache.
  
  reset_if_needed()
  cache_dir <- file.path(tempdir(), "cache_test_coincidence")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # 1. Create a file named "red"
  cwd <- tempdir()
  bad_file <- file.path(cwd, "red")
  writeLines("content1", bad_file)
  on.exit(unlink(bad_file), add = TRUE)
  
  # 2. Function using "red" as a color string
  my_plot <- cacheFile(cache_dir, backend = "rds") %@% function(color) {
    paste("Color is", color)
  }
  
  old_wd <- getwd()
  setwd(cwd)
  on.exit(setwd(old_wd), add = TRUE)
  
  # Run 1
  res1 <- my_plot("red")
  expect_equal(res1, "Color is red")
  
  # 3. Modify the file "red"
  Sys.sleep(1.1)
  writeLines("content2", bad_file)
  
  # Run 2
  # Argument "red" is same. Code is same.
  # But file "red" changed -> Cache Miss (Safe behavior)
  res2 <- my_plot("red")
  
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files, 2)
})

# --------------------------------------------------------#
# Helper: Setup Cache
# --------------------------------------------------------#
setup_cache <- function(name) {
  d <- file.path(tempdir(), name)
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d, showWarnings = FALSE)
  d
}

# --------------------------------------------------------#
test_that("Robustness: 'Touching' a file (mtime change) does NOT invalidate cache if content is identical", {
  
  cache_dir <- setup_cache("robust_touch")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  f_path <- file.path(tempdir(), "touch_me.txt")
  cat("Important Data", file = f_path)
  on.exit(unlink(f_path), add = TRUE)
  
  # Define the processor to return a LIST containing data + generation time
  proc <- cacheFile(cache_dir, backend="rds") %@% function(x) {
    list(
      data = readLines(x),
      generated_at = Sys.time() # This changes every time the function effectively runs
    )
  }
  
  # --- Run 1 ---
  result_1 <- proc(f_path)
  expect_equal(result_1$data, "Important Data")
  
  # ACTION: Update mtime, keep content same
  # Sleep ensures that if it DOES re-run, Sys.time() will definitely be different
  Sys.sleep(1.5) 
  Sys.setFileTime(f_path, Sys.time())
  
  # --- Run 2 ---
  result_2 <- proc(f_path)
  
  # 1. Content should still be correct
  expect_equal(result_2$data, "Important Data")
  
  # 2. The timestamp should match Run 1. 
  # If the cache was invalidated by the touch, this would be a new time.
  expect_equal(result_2$generated_at, result_1$generated_at)
})

# --------------------------------------------------------#
test_that("Sensitivity: Changing content (even with same size) invalidates cache", {

  cache_dir <- setup_cache("robust_content")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  f_path <- file.path(tempdir(), "change_me.txt")
  # Write 3 bytes
  cat("abc", file = f_path)
  on.exit(unlink(f_path), add = TRUE)
  
  # Define processor: Returns a list containing the data AND a generation timestamp
  proc <- cacheFile(cache_dir) %@% function(x) {
    list(
      content = readLines(x),
      # If this function runs again, this time/ID will change
      run_id = as.numeric(Sys.time()) 
    )
  }
  
  # --- Run 1 ---
  res1 <- proc(f_path)
  expect_equal(res1$content, "abc")
  
  # ACTION: Change content, but keep size identical (3 bytes)
  # 'abc' -> 'xyz'
  Sys.sleep(1.1) # Ensure mtime updates so the caching logic sees a file change
  cat("xyz", file = f_path)
  
  # --- Run 2 ---
  # Logic: mtime changed -> Re-hash content -> Content changed -> New Hash -> Cache MISS -> Re-run
  res2 <- proc(f_path)
  
  # 1. Verify we got the new content
  expect_equal(res2$content, "xyz")
  
  # 2. Verify the function actually re-executed (Run IDs should differ)
  # If the cache had incorrectly served the old result, res2$run_id would equal res1$run_id
  expect_false(res1$run_id == res2$run_id) 
})

# --------------------------------------------------------#
test_that("Sampling: Header changes are detected in large files (>64KB)", {

  cache_dir <- setup_cache("robust_sampling")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  f_path <- file.path(tempdir(), "large_file.bin")
  
  # Create 200KB file (larger than the 64KB block size)
  # Write random bytes
  set.seed(42)
  bytes <- as.raw(sample(0:255, 200 * 1024, replace=TRUE))
  writeBin(bytes, f_path)
  on.exit(unlink(f_path), add = TRUE)
  
  # Define processor: Return size AND a unique run ID
  proc <- cacheFile(cache_dir, backend = "rds") %@% function(x) {
    list(
      size = file.size(x),
      # This ID proves the function executed right now
      run_id = as.numeric(Sys.time()) 
    )
  }
  
  # --- Run 1 ---
  r1 <- proc(f_path)
  
  # ACTION: Modify the HEADER (first byte)
  # The probabilistic hasher ALWAYS reads the first block, so this must be detected.
  con <- file(f_path, "r+b")
  seek(con, 0)
  writeBin(as.raw(0x00), con) # Change first byte to 0
  close(con)
  Sys.setFileTime(f_path, Sys.time()) # Ensure mtime update
  
  # --- Run 2 ---
  r2 <- proc(f_path)
  
  # 1. The file size is technically identical (in-place overwrite)
  expect_equal(r2$size, r1$size)
  
  # 2. Crucial Check: Did we re-run?
  # If the cache correctly detected the header change, it counts as a "Miss" and re-runs.
  # Therefore, the timestamp (run_id) must be strictly greater/different.
  expect_false(r1$run_id == r2$run_id)
})

# --------------------------------------------------------#
test_that("Determinism: The same file always produces the same hash", {
  getOption("cacheR.backend", "rds")

  cache_dir <- setup_cache("robust_determinism")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  f_path <- file.path(tempdir(), "deterministic.txt")
  cat("static content", file = f_path)
  on.exit(unlink(f_path), add = TRUE)
  
  # We clear the internal memoization cache to force full re-hashing
  if (exists(".file_state_cache")) {
    rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
  }
  
  proc <- cacheFile(cache_dir) %@% function(x) x
  
  # Run 1
  proc(f_path)
  hash1 <- list.files(cache_dir, pattern = "\\.rds$")[1]
  
  # Clear Cache Directory completely
  unlink(list.files(cache_dir, full.names = TRUE))
  
  # Clear Memory Cache again
  if (exists(".file_state_cache")) {
    rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
  }
  
  # Run 2 on same file
  proc(f_path)
  hash2 <- list.files(cache_dir, pattern = "\\.rds$")[1]
  
  expect_equal(hash1, hash2)
})

# --------------------------------------------------------#

library(testthat)

# Helper: Clear memoization cache to ensure tests run fresh
reset_mem_cache <- function() {
  if (exists(".file_state_cache", envir = .GlobalEnv)) {
    rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
  }
}

# --------------------------------------------------------#
test_that("Automatic Detection: invalidates when number of files changes", {
  reset_mem_cache()
  cache_dir <- file.path(tempdir(), "cache_auto_detect")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)

  input_dir <- file.path(tempdir(), "input_files")
  unlink(input_dir, recursive = TRUE)
  dir.create(input_dir, showWarnings = FALSE)

  # Start with 1 file
  file.create(file.path(input_dir, "file1.txt"))

  # FIX: Explicitly set backend="rds" so expectations on .rds files work
  f <- cacheFile(cache_dir, backend = "rds") %@% function(path) length(list.files(path))

  n1 <- f(input_dir)
  expect_equal(n1, 1L)

  # Add 2nd file
  Sys.sleep(1.1) # ensure mtime tick
  file.create(file.path(input_dir, "file2.txt"))
  
  n2 <- f(input_dir)
  expect_equal(n2, 2L)
  
  # Check explicitly for .rds files
  expect_true(length(list.files(cache_dir, pattern = "\\.rds$")) >= 2)
})

# --------------------------------------------------------#
test_that("Edge Case: The 'Coincidence' False Positive", {
  reset_mem_cache()
  cache_dir <- file.path(tempdir(), "cache_coincidence")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  
  cwd <- tempdir()
  bad_file <- file.path(cwd, "red")
  writeLines("content1", bad_file)
  on.exit(unlink(bad_file), add = TRUE)
  
  # FIX: Explicitly set backend="rds"
  my_plot <- cacheFile(cache_dir, backend = "rds") %@% function(color) {
    paste("Color is", color)
  }
  
  old_wd <- getwd()
  setwd(cwd)
  on.exit(setwd(old_wd), add = TRUE)
  
  # Run 1
  res1 <- my_plot("red")
  expect_equal(res1, "Color is red")
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 1)
  
  # Run 2: Modify file "red"
  Sys.sleep(1.1)
  writeLines("content2", bad_file)
  
  res2 <- my_plot("red")
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 2)
})

# --------------------------------------------------------#
test_that("Robustness: Touching a file (mtime change) does NOT invalidate if content is identical", {
  reset_mem_cache()
  cache_dir <- file.path(tempdir(), "cache_robust_touch")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  
  f_path <- file.path(tempdir(), "touch_me.txt")
  cat("data", file = f_path)
  
  # FIX: Explicitly set backend="rds"
  proc <- cacheFile(cache_dir, backend = "rds") %@% function(x) {
    readLines(x)
  }
  
  # Run 1
  proc(f_path)
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 1)

  
  # Touch file
  Sys.sleep(1.1)
  Sys.setFileTime(f_path, Sys.time())
  
  # Run 2 (Should Hit Cache)
  proc(f_path)
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 1)


})

# --------------------------------------------------------#
test_that("Sampling: Header changes are detected in large files (>64KB)", {
  reset_mem_cache()
  cache_dir <- file.path(tempdir(), "cache_sampling")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  
  f_path <- file.path(tempdir(), "large_file.bin")
  on.exit(unlink(f_path), add = TRUE)
  
  # Create 200KB file
  set.seed(42)
  bytes <- as.raw(sample(0:255, 200 * 1024, replace=TRUE))
  writeBin(bytes, f_path)
  
  # FIX: Explicitly set backend="rds"
  proc <- cacheFile(cache_dir, backend = "rds") %@% function(x) file.size(x)
  
  # Run 1
  r1 <- proc(f_path)
  files1 <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files1, 1)
  
  # Modify first byte (Header)
  con <- file(f_path, "r+b")
  seek(con, 0)
  writeBin(as.raw(0x00), con)
  close(con)
  Sys.setFileTime(f_path, Sys.time())
  
  # Run 2
  r2 <- proc(f_path)
  
  files2 <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files2, 2)
})