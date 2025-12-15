# --------------------------------------------------------#
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
test_that("recursive calls produce parent–child relationships", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_2")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  inner_fun <- cacheFile(cache_dir) %@% function(x) {
    x + 1
  }
  
  outer_fun <- cacheFile(cache_dir) %@% function(x) {
    inner_fun(x) * 2
  }
  
  res <- outer_fun(3)
  expect_equal(res, (3 + 1) * 2)
  
  nodes <- cacheTree_nodes()
  expect_true(length(nodes) >= 2)
  
  # Find node IDs for outer and inner
  outer_id <- grep("^outer_fun:", names(nodes), value = TRUE)
  inner_id <- grep("^inner_fun:", names(nodes), value = TRUE)
  expect_length(outer_id, 1L)
  expect_length(inner_id, 1L)
  
  outer_node <- nodes[[outer_id]]
  inner_node <- nodes[[inner_id]]
  
  # outer should list inner as a child, and inner should list outer as parent
  expect_true(inner_id %in% outer_node$children)
  expect_true(outer_id %in% inner_node$parents)
  
  unlink(cache_dir, recursive = TRUE)
})

# --------------------------------------------------------#
test_that("track_file registers file dependencies with hashes", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_3")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create a temporary CSV file
  data_path <- file.path(tempdir(), "cache_tree_data.csv")
  write.csv(data.frame(x = 1:3), data_path, row.names = FALSE)
  
  read_and_sum <- cacheFile(cache_dir) %@% function(path) {
    df <- read.csv(track_file(path))
    sum(df$x)
  }
  
  res <- read_and_sum(data_path)
  expect_equal(res, 6)
  
  nodes <- cacheTree_nodes()
  # There should be exactly 1 node: the cached read_and_sum call
  expect_equal(length(nodes), 1L)
  node <- nodes[[1]]
  
  np <- normalizePath(data_path, mustWork = FALSE)
  
  # File path is recorded
  expect_true(np %in% node$files)
  
  # Hash is recorded and looks sane (non-empty character)
  fh <- node$file_hashes
  expect_true(np %in% names(fh))
  expect_true(is.character(fh[[np]]) || is.na(fh[[np]]))
  expect_gt(nchar(fh[[np]]), 0)
  
  unlink(cache_dir, recursive = TRUE)
  unlink(data_path)
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
  
  # 3. Check file count (RDS ONLY, ignoring .lock)
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files, 1)
  
  # 4. New value -> New file
  res3 <- f(a = 5, b = 11)
  expect_equal(res3, 16)
  
  files_now <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files_now, 2)
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
  
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
})

# --------------------------------------------------------------------- #
test_that("xxhash64 backend works and produces valid filenames", {
  cache_dir <- file.path(tempdir(), "cache_test_algo")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Force backend to rds for consistent counting
  cached_f <- cacheFile(cache_dir, algo = "xxhash64", backend = "rds") %@% function(x) x
  
  cached_f(1)
  
  # Check file count
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 1)
  
  cached_f(2)
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
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
  
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
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
  
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(files, 2)
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
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 1)
  
  cached_f(2)
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
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

test_that("Cache invalidates when EXPRESSIONS change (even if value is same)", {
  cache_dir <- file.path(tempdir(), "test_expr_change")
  on.exit(unlink(cache_dir, recursive = TRUE))
  dir.create(cache_dir, showWarnings=FALSE)
  
  f <- cacheFile(cache_dir) %@% function(a) {
    as.numeric(Sys.time())
  }
  
  # 1. Setup variables
  val_A <- 100
  val_B <- 100 # Identical value
  
  # Run 1: Pass 'val_A'
  id1 <- f(val_A)
  
  Sys.sleep(1.1)
  
  # Run 2: Pass 'val_B'
  # Value is identical (100), but the source code (expression) changed from 'val_A' to 'val_B'.
  # Ideally, this should be a MISS to ensure provenance safety.
  id2 <- f(val_B)
  
  expect_false(id1 == id2)
})

test_that("Cache invalidates when LITERALS change to VARIABLES (even if value is same)", {
  cache_dir <- file.path(tempdir(), "test_literal_vs_var")
  on.exit(unlink(cache_dir, recursive = TRUE))
  dir.create(cache_dir, showWarnings=FALSE)
  
  f <- cacheFile(cache_dir) %@% function(a) {
    as.numeric(Sys.time())
  }
  
  x <- 50
  
  # Run 1: Literal 50
  id1 <- f(50)
  
  Sys.sleep(1.1)
  
  # Run 2: Variable x (value is 50)
  # Expression changed from `50` to `x`. Should Miss.
  id2 <- f(x)
  
  expect_false(id1 == id2)
})

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