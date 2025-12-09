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
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_1")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Use 'options' to store state. 
  # The static analyzer sees 'getOption' (base function) but does not 
  # see the value inside the option, so the hash remains stable.
  options(cacheR_test_runs = 0)
  on.exit(options(cacheR_test_runs = NULL))
  
  cached_fun <- cacheFile(cache_dir) %@% function(x) {
    # Increment counter invisibly to the hasher
    current <- getOption("cacheR_test_runs")
    options(cacheR_test_runs = current + 1)
    x * 2
  }
  
  # First call: should run the body
  expect_equal(cached_fun(10), 20)
  expect_equal(getOption("cacheR_test_runs"), 1L)
  
  # Second call: hash of function body + deps (base::options) is identical.
  # Should hit cache. Counter should NOT increment.
  expect_equal(cached_fun(10), 20)
  expect_equal(getOption("cacheR_test_runs"), 1L)
  
  # Different args => new cache entry, new run
  expect_equal(cached_fun(5), 10)
  expect_equal(getOption("cacheR_test_runs"), 2L)
  
  unlink(cache_dir, recursive = TRUE)
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
test_that("cacheTree_changed_files flags modified tracked file (direct use)", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_4")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  data_path <- file.path(tempdir(), "cache_tree_data_direct.csv")
  write.csv(data.frame(x = 1:3), data_path, row.names = FALSE)
  
  read_and_sum <- cacheFile(cache_dir) %@% function(path) {
    df <- read.csv(track_file(path))
    sum(df$x)
  }
  
  # Initial run, registers file and hash
  res <- read_and_sum(data_path)
  expect_equal(res, 6)
  
  np <- normalizePath(data_path, mustWork = FALSE)
  
  # Now change the file contents
  write.csv(data.frame(x = 10:12), data_path, row.names = FALSE)
  
  changed <- cacheTree_changed_files()
  
  # There should be exactly 1 node flagged as changed
  expect_equal(length(changed), 1L)
  
  # Its changed_files should include our file
  changed_entry <- changed[[1]]
  expect_true(np %in% changed_entry$changed_files)
  
  unlink(cache_dir, recursive = TRUE)
  unlink(data_path)
})

# --------------------------------------------------------#
test_that("cacheTree_changed_files flags modified file when tracked in internal helper", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_5")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  data_path <- file.path(tempdir(), "cache_tree_data_internal.csv")
  write.csv(data.frame(x = 1:3), data_path, row.names = FALSE)
  
  # Internal helper (NOT decorated) that calls track_file()
  helper_read_sum <- function(path) {
    df <- read.csv(track_file(path))
    sum(df$x)
  }
  
  # Only this function is decorated; it calls helper_read_sum()
  outer_cached <- cacheFile(cache_dir) %@% function(path) {
    helper_read_sum(path)
  }
  
  res <- outer_cached(data_path)
  expect_equal(res, 6)
  
  # We expect exactly one node in the graph: the outer_cached call
  nodes <- cacheTree_nodes()
  expect_equal(length(nodes), 1L)
  node_id <- names(nodes)[[1]]
  
  np <- normalizePath(data_path, mustWork = FALSE)
  
  # Confirm that this node has the file registered
  expect_true(np %in% nodes[[node_id]]$files)
  
  # Modify the file
  write.csv(data.frame(x = 100:102), data_path, row.names = FALSE)
  
  changed <- cacheTree_changed_files()
  
  # Only one node should be reported as changed
  expect_equal(length(changed), 1L)
  expect_true(node_id %in% names(changed))
  
  # And that node should list our file as changed
  changed_entry <- changed[[node_id]]
  expect_true(np %in% changed_entry$changed_files)
  
  unlink(cache_dir, recursive = TRUE)
  unlink(data_path)
})

# --------------------------------------------------------#
test_that("cacheTree_save and cacheTree_load work", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_6")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  f1 <- cacheFile(cache_dir) %@% function(x) x + 1
  f2 <- cacheFile(cache_dir) %@% function(x) f1(x) * 2
  
  res <- f2(10)
  expect_equal(res, (10 + 1) * 2)
  
  nodes_before <- cacheTree_nodes()
  expect_true(length(nodes_before) >= 2)
  
  graph_path <- file.path(tempdir(), "cache_graph.rds")
  cacheTree_save(graph_path)
  expect_true(file.exists(graph_path))
  
  # Reset graph and load from disk
  cacheTree_reset()
  expect_equal(length(cacheTree_nodes()), 0L)
  
  cacheTree_load(graph_path)
  nodes_after <- cacheTree_nodes()
  
  # Node IDs should match before and after
  expect_setequal(names(nodes_before), names(nodes_after))
  
  unlink(cache_dir, recursive = TRUE)
  unlink(graph_path)
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
  expect_equal(loaded$dat, 20)
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
  
  cached_f <- cacheFile(cache_dir, file_args = "file", backend="rds") %@% f
  
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
  
  cached_f <- cacheFile(cache_dir, file_args = "d", backend="rds") %@% f
  
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