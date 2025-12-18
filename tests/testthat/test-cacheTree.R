library(testthat)
library(digest)

# --- Helper to clean state between tests ---
setup_env <- function() {
  cacheTree_reset()
  # Clear file memoization cache
  rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
}

teardown_env <- function() {
  cacheTree_reset()
}

# =========================================================================
# TESTS
# =========================================================================

test_that("cacheTree_reset clears the environment", {
  setup_env()
  
  # Manually inject state using the new internal environment
  assign("node1", list(id="node1"), envir = .graph_cache$nodes)
  .graph_cache$call_stack <- c("node1")
  
  expect_length(ls(.graph_cache$nodes), 1)
  expect_length(.graph_cache$call_stack, 1)
  
  cacheTree_reset()
  
  expect_length(ls(.graph_cache$nodes), 0)
  expect_length(.graph_cache$call_stack, 0)
})

# --------------------------------------------------------#

test_that("Node registration works and handles parent-child linking", {
  setup_env()
  on.exit(teardown_env())
  
  # 1. Register Root Node A
  # Using internal helper .register_node
  .register_node("A", "function", label="funcA")
  
  # Manually push A to stack (simulating the function call)
  .graph_cache$call_stack <- c("A")
  
  # 2. Register Node B (Child of A)
  .register_node("B", "function", label="funcB")
  
  # Manually register the edge (cacheFile does this automatically, testing logic here)
  .register_edge(from = "A", to = "B")
  
  nodes <- cacheTree_nodes()
  
  expect_true("A" %in% names(nodes))
  expect_true("B" %in% names(nodes))
  
  # Check A properties
  expect_equal(nodes$A$children, "B")
  expect_equal(length(nodes$A$parents), 0) # Should be empty character
  
  # Check B properties
  expect_equal(nodes$B$parents, "A")
  expect_equal(length(nodes$B$children), 0)
})


# --------------------------------------------------------#

test_that("cacheTree_save and cacheTree_load preserve structure", {
  setup_env()
  on.exit(teardown_env())
  
  # Create a dummy graph
  .register_node("N1", "function", label="f1")
  
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)
  
  cacheTree_save(tmp_file)
  
  # Modify state then load back
  cacheTree_reset()
  expect_length(cacheTree_nodes(), 0)
  
  cacheTree_load(tmp_file)
  nodes <- cacheTree_nodes()
  
  expect_true("N1" %in% names(nodes))
  expect_equal(nodes$N1$label, "f1")
})


# --------------------------------------------------------#

test_that("probabilistic_file_hash is deterministic", {
  # Create a dummy file larger than block size to trigger sampling
  tf <- tempfile()
  on.exit(unlink(tf))
  
  # Write 200KB of random data
  writeBin(as.raw(sample(0:255, 200 * 1024, replace=TRUE)), tf)
  
  h1 <- .probabilistic_file_hash(tf)
  h2 <- .probabilistic_file_hash(tf)
  
  expect_false(is.na(h1))
  expect_equal(h1, h2)
})

# --------------------------------------------------------#

test_that("fast_file_hash uses memoization but detects size changes", {
  setup_env()
  on.exit(teardown_env())
  
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  
  cat("Hello", file = tf)
  
  # First hash
  h1 <- .fast_file_hash(tf)
  
  # Verify it is stored in the cache
  # Note: normalizePath is used as key in .file_state_cache
  norm_tf <- normalizePath(tf, winslash = "/", mustWork = FALSE)
  expect_true(exists(norm_tf, envir = .file_state_cache))
  
  # Modify file content (and size)
  cat("Hello World", file = tf)
  
  h2 <- .fast_file_hash(tf)
  
  expect_false(identical(h1, h2))
})

# --------------------------------------------------------#

test_that("track_file associates file with current node", {
  setup_env()
  on.exit(teardown_env())
  
  tf <- tempfile()
  cat("data", file = tf)
  on.exit(unlink(tf), add = TRUE)
  
  # Setup Node and simulate being inside a function call
  .register_node("NodeX", "function", label="fn")
  .graph_cache$call_stack <- c("NodeX")
  
  # Call track_file
  track_file(tf)
  
  node <- cacheTree_nodes()$NodeX
  norm_path <- normalizePath(tf, mustWork = FALSE, winslash = "/")
  
  expect_true(norm_path %in% node$files)
  expect_true(norm_path %in% names(node$file_hashes))
  expect_false(is.na(node$file_hashes[[norm_path]]))
})

test_that("cacheTree_for_file finds relevant nodes", {
  setup_env()
  on.exit(teardown_env())
  
  tf <- tempfile()
  cat("x", file=tf)
  on.exit(unlink(tf), add = TRUE)
  
  # Setup context
  .register_node("N1", "function", label="f")
  .graph_cache$call_stack <- c("N1")
  
  track_file(tf)
  
  res <- cacheTree_for_file(tf)
  expect_length(res, 1)
  expect_equal(res[[1]]$id, "N1")
  
  res_fail <- cacheTree_for_file("non/existent/path")
  expect_length(res_fail, 0)
})

# --------------------------------------------------------#

test_that("cacheR_default_dir creates directory if missing", {
  tmp_dir <- file.path(tempdir(), ".cacheR_test")
  if(dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
  
  # Mock getOption using withr
  withr::with_options(list(cacheR.dir = tmp_dir), {
    res <- cacheR_default_dir()
    expect_equal(res, tmp_dir)
    expect_true(dir.exists(tmp_dir))
  })
  
  unlink(tmp_dir, recursive = TRUE)
})

test_that("cachePrune deletes old files", {
  tmp_dir <- file.path(tempdir(), "prune_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  
  # Create "Old" file
  f_old <- file.path(tmp_dir, "old.rds")
  cat("old", file = f_old)
  
  # Manually backdate mtime to 31 days ago
  old_time <- Sys.time() - (31 * 24 * 3600)
  Sys.setFileTime(f_old, old_time)
  
  # Create "New" file
  f_new <- file.path(tmp_dir, "new.rds")
  cat("new", file = f_new)
  
  # Run Prune (threshold 30 days)
  cachePrune(tmp_dir, days_old = 30)
  
  expect_false(file.exists(f_old))
  expect_true(file.exists(f_new))
})