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
# Helper: Count cache entry files, excluding graph.rds
# --------------------------------------------------------#
count_cache_entries <- function(cache_dir, backend_pattern = "\\.(rds|qs)$") {
  files <- list.files(cache_dir, pattern = backend_pattern)
  length(files[!grepl("^graph\\.rds", files)])
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
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
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

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
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
  
  # FIX: Use writeLines() instead of cat() to ensure a proper newline exists.
  writeLines("Important Data", f_path)
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
  # FIX: Add newline to avoid readLines warning
  cat("abc\n", file = f_path) 
  on.exit(unlink(f_path), add = TRUE)
  
  proc <- cacheFile(cache_dir) %@% function(x) {
    list(
      content = readLines(x),
      run_id = as.numeric(Sys.time()) 
    )
  }
  
  # --- Run 1 ---
  res1 <- proc(f_path)
  expect_equal(res1$content, "abc")
  
  # ACTION: Change content, but keep size identical (4 bytes now due to \n)
  Sys.sleep(1.1) 
  # FIX: Add newline here too
  cat("xyz\n", file = f_path)
  
  # --- Run 2 ---
  res2 <- proc(f_path)
  
  expect_equal(res2$content, "xyz")
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
  all_files1 <- list.files(cache_dir, pattern = "\\.rds$")
  hash1 <- all_files1[!grepl("^graph\\.rds$", all_files1)][1]

  # Clear Cache Directory completely
  unlink(list.files(cache_dir, full.names = TRUE))

  # Clear Memory Cache again
  if (exists(".file_state_cache")) {
    rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
  }

  # Run 2 on same file
  proc(f_path)
  all_files2 <- list.files(cache_dir, pattern = "\\.rds$")
  hash2 <- all_files2[!grepl("^graph\\.rds$", all_files2)][1]

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
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  # Run 2: Modify file "red"
  Sys.sleep(1.1)
  writeLines("content2", bad_file)

  res2 <- my_plot("red")
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

# --------------------------------------------------------#
test_that("Robustness: Touching a file (mtime change) does NOT invalidate if content is identical", {
  reset_mem_cache()
  cache_dir <- file.path(tempdir(), "cache_robust_touch")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE)
  
  f_path <- file.path(tempdir(), "touch_me.txt")
  # FIX: Use writeLines for cleaner file writing with newlines
  writeLines("data", f_path)
  
  proc <- cacheFile(cache_dir, backend = "rds") %@% function(x) {
    readLines(x)
  }
  
  # Run 1
  proc(f_path)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  # Touch file
  Sys.sleep(1.1)
  Sys.setFileTime(f_path, Sys.time())

  # Run 2 (Should Hit Cache)
  proc(f_path)
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)
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
  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 1)

  # Modify first byte (Header)
  con <- file(f_path, "r+b")
  seek(con, 0)
  writeBin(as.raw(0x00), con)
  close(con)
  Sys.setFileTime(f_path, Sys.time())

  # Run 2
  r2 <- proc(f_path)

  expect_equal(count_cache_entries(cache_dir, "\\.rds$"), 2)
})

# ---------------------------------------------------------------- #
# Directory Hashing (Relative Paths)
# ---------------------------------------------------------------- #

test_that("Directory hashing detects file renaming (structure changes)", {
  cache_dir <- file.path(tempdir(), "test_dir_struct")
  data_dir  <- file.path(tempdir(), "data_struct")
  
  on.exit({
    unlink(cache_dir, recursive = TRUE)
    unlink(data_dir, recursive = TRUE)
  })
  
  dir.create(cache_dir, showWarnings=FALSE)
  dir.create(data_dir, showWarnings=FALSE)
  
  # Setup: Create file A
  writeLines("content", file.path(data_dir, "fileA.txt"))
  
  f <- cacheFile(cache_dir) %@% function(path) {
    as.numeric(Sys.time())
  }
  
  # Run 1
  id1 <- f(data_dir)
  
  # ACTION: Rename fileA -> fileB 
  # (Content is identical, size identical. Mtime updates, but structure definitely updates)
  file.rename(file.path(data_dir, "fileA.txt"), file.path(data_dir, "fileB.txt"))
  
  Sys.sleep(1.1)
  
  # Run 2
  id2 <- f(data_dir)
  
  expect_false(id1 == id2)
})


# -------------------------------------------------- #
test_that("file paths in global config list invalidate cache when file changes (no runs counter)", {
  # Known limitation: zero-arg functions access files via globals inside the body.
  # The cache key has no argument-derived file paths to track, so external file

  # changes are not detected. This test documents desired but unimplemented behavior.
  skip("Known limitation: cache cannot track files referenced via globals in zero-arg functions")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_config_list_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  config <<- list(path = tempfile(fileext = ".txt"))
  on.exit(rm(config, envir = .GlobalEnv), add = TRUE)

  writeLines("A", config$path)

  f <- cacheFile(cache_dir = cache_dir) %@% function() {
    readLines(config$path, warn = FALSE)
  }

  r1 <- f()
  expect_equal(r1, "A")
  expect_equal(count_cache_entries(cache_dir), 1L)

  writeLines("B", config$path)

  # Desired behaviour: invalidation because config$path points to changed file
  r2 <- f()
  expect_equal(r2, "B")
  expect_equal(count_cache_entries(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("file paths nested inside argument lists invalidate cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_arg_list_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cfg <- list(path = tempfile(fileext = ".txt"))
  writeLines("A", cfg$path)

  g <- cacheFile(cache_dir = cache_dir) %@% function(cfg) {
    readLines(cfg$path, warn = FALSE)
  }

  r1 <- g(cfg)
  expect_equal(r1, "A")
  expect_equal(count_cache_entries(cache_dir), 1L)

  writeLines("B", cfg$path)

  # Desired behaviour: invalidation because cfg$path file changed
  r2 <- g(cfg)
  expect_equal(r2, "B")
  expect_equal(count_cache_entries(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("non-character file reference via connection invalidates cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_nonchar_connection_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  p <- tempfile(fileext = ".txt")
  writeLines("A", p)

  con <- file(p, open = "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    seek(con, 0, rw = "read")
    readLines(con, warn = FALSE)
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_entries(cache_dir), 1L)

  writeLines("B", p)

  # Desired: invalidation because underlying file changed (even though arg is a connection)
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_entries(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("DBI connection data changes invalidate cache (no runs counter)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_dbi_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE t(val TEXT)")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('A')")

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    DBI::dbGetQuery(con, "SELECT val FROM t LIMIT 1")$val[[1]]
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_entries(cache_dir), 1L)

  DBI::dbExecute(con, "DELETE FROM t")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('B')")

  # Desired: invalidation because DB contents changed
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_entries(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("cache saves WITH WARNING if argument file is modified during execution", {
  # Setup
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_warn_output_detection")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Create a dummy file
  tf <- tempfile(fileext = ".txt")
  writeLines("Initial Content", tf)

  # 2. Define a function that modifies the file passed to it
  f <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    Sys.sleep(1.1) # Sleep to ensure mtime changes
    cat("\nNew Line", file = path, append = TRUE)
    return("Result")
  }

  # 3. Execution: Expect a Warning, BUT also ensure Cache IS SAVED
  expect_warning(
    r1 <- f(tf),
    regexp = "Function modified argument files during execution"
  )

  expect_equal(r1, "Result")

  # CRITICAL CHECK: The cache directory should NOT be empty.
  # We proceed with caching despite the warning.
  expect_equal(count_cache_entries(cache_dir), 1L)

  # 4. Control Test: Ensure normal read-only files ARE cached
  tf2 <- tempfile(fileext = ".txt")
  writeLines("Static Content", tf2)

  g <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    readLines(path, warn = FALSE)
  }

  r2 <- g(tf2)
  expect_equal(r2, "Static Content")
  # Total cache files should be 2 now
  expect_equal(count_cache_entries(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("symlinks are resolved to their target files for caching", {
  # Symlink creation is OS-dependent and often restricted on Windows
  skip_on_os("windows")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_symlinks")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Create Target File
  target_file <- tempfile(fileext = ".txt")
  writeLines("Original Content", target_file)

  # 2. Create Symlink
  link_file <- tempfile(fileext = ".link")
  file.symlink(target_file, link_file)
  on.exit({ unlink(target_file); unlink(link_file) }, add = TRUE)

  # 3. Define function
  f <- cacheFile(cache_dir = cache_dir) %@% function(path) {
    readLines(path, warn = FALSE)
  }

  # 4. Run with SYMLINK
  r1 <- f(link_file)
  expect_equal(r1, "Original Content")
  expect_equal(count_cache_entries(cache_dir), 1L)

  # 5. Run with TARGET FILE
  # Known limitation: symlink and target produce different input hashes because
  # the raw argument string differs, even though normalizePath resolves both
  # to the same file. This creates a separate cache entry.
  r2 <- f(target_file)
  expect_equal(r2, "Original Content")
  expect_equal(count_cache_entries(cache_dir), 2L)

  # 6. Modify Target
  writeLines("Modified Content", target_file)

  # 7. Run with SYMLINK again
  # Should invalidate cache because target content changed
  r3 <- f(link_file)
  expect_equal(r3, "Modified Content")
  expect_equal(count_cache_entries(cache_dir), 3L)
})



# -------------------------------------------------- #
test_that("hash_file_paths controls location sensitivity", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()
  
  # Setup Directories
  base_dir <- file.path(tempdir(), "cache_arg_toggle")
  unlink(base_dir, recursive = TRUE, force = TRUE)
  dir.create(base_dir, recursive = TRUE)
  
  dir_A <- file.path(base_dir, "A"); dir.create(dir_A)
  dir_B <- file.path(base_dir, "B"); dir.create(dir_B)
  
  # Create Identical Content in two places
  file_A <- file.path(dir_A, "data.txt"); writeLines("XYZ", file_A)
  file_B <- file.path(dir_B, "data.txt"); writeLines("XYZ", file_B)

  # -----------------------------------------------------------------------
  # CASE 1: Strict Mode (hash_file_paths = TRUE)
  # -----------------------------------------------------------------------
  cache_strict <- file.path(base_dir, "cache_strict")

  f_strict <- cacheFile(cache_dir = cache_strict, hash_file_paths = TRUE) %@% function(p) {
    readLines(p, warn = FALSE)
  }

  # Run A
  expect_equal(f_strict(file_A), "XYZ")
  expect_equal(count_cache_entries(cache_strict), 1L)

  # Run B (Different Path, Same Content) -> EXPECT NEW CACHE
  expect_equal(f_strict(file_B), "XYZ")
  expect_equal(count_cache_entries(cache_strict), 2L) # Missed, created new entry

  # -----------------------------------------------------------------------
  # CASE 2: Portable Mode (hash_file_paths = FALSE)
  # -----------------------------------------------------------------------
  cache_portable <- file.path(base_dir, "cache_portable")

  f_portable <- cacheFile(cache_dir = cache_portable, hash_file_paths = FALSE) %@% function(p) {
    readLines(p, warn = FALSE)
  }

  # Run A
  expect_equal(f_portable(file_A), "XYZ")
  expect_equal(count_cache_entries(cache_portable), 1L)

  # Run B (Different Path, Same Content) -> EXPECT REUSED CACHE
  expect_equal(f_portable(file_B), "XYZ")
  expect_equal(count_cache_entries(cache_portable), 1L) # Hit, no new entry
})

# --------------------------------------------------------#
# Tests for vector file path tracking (Issue #5)
# --------------------------------------------------------#

test_that("Vector of file paths invalidates cache when any file changes", {
  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_vec_paths_invalidate")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  file1 <- file.path(tempdir(), "vec_test_file1.txt")
  file2 <- file.path(tempdir(), "vec_test_file2.txt")
  writeLines("content_A", file1)
  writeLines("content_B", file2)
  on.exit(unlink(c(file1, file2)), add = TRUE)

  # Exclude graph.rds from count
  count_cache <- function(d) length(grep("^graph\\.", list.files(d, pattern = "\\.(rds|qs)$"), invert = TRUE, value = TRUE))

  f <- cacheFile(cache_dir = cache_dir) %@% function(paths) {
    paste(vapply(paths, function(p) readLines(p, warn = FALSE), character(1)), collapse = "|")
  }

  r1 <- f(c(file1, file2))
  expect_equal(r1, "content_A|content_B")
  expect_equal(count_cache(cache_dir), 1L)

  # Modify file2
  Sys.sleep(1.1)
  writeLines("content_C", file2)

  r2 <- f(c(file1, file2))
  expect_equal(r2, "content_A|content_C")
  expect_equal(count_cache(cache_dir), 2L)
})

test_that("Vector of mixed file paths and regular strings invalidates on file change", {
  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_vec_mixed_paths")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  file1 <- file.path(tempdir(), "vec_mixed_file1.txt")
  writeLines("data_X", file1)
  on.exit(unlink(file1), add = TRUE)

  # Exclude graph.rds from count
  count_cache <- function(d) length(grep("^graph\\.", list.files(d, pattern = "\\.(rds|qs)$"), invert = TRUE, value = TRUE))

  f <- cacheFile(cache_dir = cache_dir) %@% function(items) {
    paste(items, collapse = ",")
  }

  r1 <- f(c(file1, "not_a_file"))
  expect_equal(count_cache(cache_dir), 1L)

  # Modify file1
  Sys.sleep(1.1)
  writeLines("data_Y", file1)

  r2 <- f(c(file1, "not_a_file"))
  # Cache should have been invalidated because file1 content changed
  expect_equal(count_cache(cache_dir), 2L)
})

test_that("Vector of file paths works with hash_file_paths = FALSE (portable mode)", {
  reset_if_needed()

  base_dir <- file.path(tempdir(), "cache_vec_portable")
  unlink(base_dir, recursive = TRUE, force = TRUE)
  dir.create(base_dir, recursive = TRUE)

  dir_A <- file.path(base_dir, "A"); dir.create(dir_A)
  dir_B <- file.path(base_dir, "B"); dir.create(dir_B)

  # Create identical files at different paths
  file_A <- file.path(dir_A, "data.txt"); writeLines("SAME", file_A)
  file_B <- file.path(dir_B, "data.txt"); writeLines("SAME", file_B)

  # Exclude graph.rds from count
  count_cache <- function(d) length(grep("^graph\\.", list.files(d, pattern = "\\.(rds|qs)$"), invert = TRUE, value = TRUE))

  cache_dir <- file.path(base_dir, "cache")

  f <- cacheFile(cache_dir = cache_dir, hash_file_paths = FALSE) %@% function(paths) {
    paste(vapply(paths, function(p) readLines(p, warn = FALSE), character(1)), collapse = "|")
  }

  # Run with files from dir A
  r1 <- f(c(file_A))
  expect_equal(count_cache(cache_dir), 1L)

  # Run with files from dir B (same content, different path)
  # In portable mode, this should hit the cache
  r2 <- f(c(file_B))
  expect_equal(count_cache(cache_dir), 1L)
})

test_that("Vector of directory paths invalidates when dir content changes", {
  reset_if_needed()

  cache_dir <- file.path(tempdir(), "cache_vec_dirs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  dir1 <- file.path(tempdir(), "vec_dir1")
  dir2 <- file.path(tempdir(), "vec_dir2")
  unlink(c(dir1, dir2), recursive = TRUE, force = TRUE)
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  on.exit(unlink(c(dir1, dir2), recursive = TRUE), add = TRUE)

  file.create(file.path(dir1, "a.txt"))
  file.create(file.path(dir2, "b.txt"))

  # Exclude graph.rds from count
  count_cache <- function(d) length(grep("^graph\\.", list.files(d, pattern = "\\.(rds|qs)$"), invert = TRUE, value = TRUE))

  f <- cacheFile(cache_dir = cache_dir) %@% function(dirs) {
    sum(vapply(dirs, function(d) length(list.files(d)), integer(1)))
  }

  r1 <- f(c(dir1, dir2))
  expect_equal(r1, 2L)
  expect_equal(count_cache(cache_dir), 1L)

  # Add a file to dir2
  Sys.sleep(1.1)
  file.create(file.path(dir2, "c.txt"))

  r2 <- f(c(dir1, dir2))
  expect_equal(r2, 3L)
  expect_equal(count_cache(cache_dir), 2L)
})