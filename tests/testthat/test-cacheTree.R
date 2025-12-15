library(testthat)
library(digest) # Required by the source code

# Source the functions provided
# source("cacheTree.R") 

# --- Helper to clean state between tests ---
setup_env <- function() {
  cacheTree_reset()
}

teardown_env <- function() {
  cacheTree_reset()
  # Clear the file memoization cache as well
  rm(list = ls(envir = .file_state_cache), envir = .file_state_cache)
}

# =========================================================================
# TESTS
# =========================================================================

  
# --------------------------------------------------------#

  test_that("cacheTree_reset clears the environment", {
    setup_env()
    
    # Manually inject state
    assign("node1", list(id="node1"), envir = .cacheTree_env$graph)
    .cacheTree_env$call_stack <- c("node1")
    
    expect_length(ls(.cacheTree_env$graph), 1)
    expect_length(.cacheTree_env$call_stack, 1)
    
    cacheTree_reset()
    
    expect_length(ls(.cacheTree_env$graph), 0)
    expect_length(.cacheTree_env$call_stack, 0)
  })
  
# --------------------------------------------------------#

  test_that("Node registration works and handles parent-child linking", {
    setup_env()
    on.exit(teardown_env())
    
    # 1. Register Root Node
    # To register a root, stack must be empty initially, then we simulate
    # the stack management that usually happens in the main wrapper.
    # However, register_node looks at the *current* stack to find a parent.
    
    # Register Node A (Root)
    .cacheTree_register_node("A", "funcA", "hashA", "outA.rds")
    
    # Manually push A to stack (simulating the function call)
    .cacheTree_env$call_stack <- c("A")
    
    # Register Node B (Child of A)
    .cacheTree_register_node("B", "funcB", "hashB", "outB.rds")
    
    nodes <- cacheTree_nodes()
    
    expect_true("A" %in% names(nodes))
    expect_true("B" %in% names(nodes))
    
    # Check A properties
    expect_equal(nodes$A$children, "B")
    expect_equal(nodes$A$parents, character(0))
    
    # Check B properties
    expect_equal(nodes$B$parents, "A")
    expect_equal(nodes$B$children, character(0))
  })

 
# --------------------------------------------------------#
 
  test_that("cacheTree_save and cacheTree_load preserve structure", {
    setup_env()
    on.exit(teardown_env())
    
    # Create a dummy graph
    .cacheTree_register_node("N1", "f1", "h1", "o1")
    
    tmp_file <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp_file), add = TRUE)
    
    cacheTree_save(tmp_file)
    
    # Modify state then load back
    cacheTree_reset()
    expect_length(cacheTree_nodes(), 0)
    
    cacheTree_load(tmp_file)
    nodes <- cacheTree_nodes()
    
    expect_true("N1" %in% names(nodes))
    expect_equal(nodes$N1$fname, "f1")
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
    expect_true(tf %in% ls(.file_state_cache))
    
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
    
    # Setup Node
    .cacheTree_env$call_stack <- c("NodeX")
    .cacheTree_register_node("NodeX", "fn", "h", "o")
    
    track_file(tf)
    
    node <- cacheTree_nodes()$NodeX
    norm_path <- normalizePath(tf)
    
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
    
    .cacheTree_env$call_stack <- c("N1")
    .cacheTree_register_node("N1", "f", "h", "o")
    track_file(tf)
    
    res <- cacheTree_for_file(tf)
    expect_length(res, 1)
    expect_equal(res[[1]]$id, "N1")
    
    res_fail <- cacheTree_for_file("non/existent/path")
    expect_length(res_fail, 0)
  })

  
# --------------------------------------------------------#

  test_that("cacheTree_changed_files detects modification and deletion", {
    setup_env()
    on.exit(teardown_env())
    
    f_mod <- tempfile() # To be modified
    f_del <- tempfile() # To be deleted
    f_ok  <- tempfile() # To stay same
    
    cat("original", file = f_mod)
    cat("delete_me", file = f_del)
    cat("steady", file = f_ok)
    
    on.exit(unlink(c(f_mod, f_del, f_ok)), add = TRUE)
    
    # Register Node and track files
    .cacheTree_env$call_stack <- c("N_Check")
    .cacheTree_register_node("N_Check", "fn", "h", "o")
    
    track_file(f_mod)
    track_file(f_del)
    track_file(f_ok)
    
    # Initial check: nothing should be changed immediately
    # Note: fast_file_hash caches the state, so immediate check compares cache to disk
    changes_init <- cacheTree_changed_files()
    expect_length(changes_init, 0)
    
    # --- ACTION: Modify / Delete ---
    
    # Modify f_mod
    Sys.sleep(1.1) # Ensure mtime changes on FS with low resolution
    cat("changed content", file = f_mod)
    
    # Delete f_del
    unlink(f_del)
    
    # --- Check Changes ---
    changes <- cacheTree_changed_files()
    
    expect_true("N_Check" %in% names(changes))
    affected_files <- changes$N_Check$changed_files
    
    # Check normalized paths
    expect_true(normalizePath(f_mod) %in% affected_files)
    expect_true(normalizePath(f_del) %in% affected_files)
    expect_false(normalizePath(f_ok) %in% affected_files)
  })
  
# --------------------------------------------------------#

  test_that("cacheR_default_dir creates directory if missing", {
    tmp_dir <- file.path(tempdir(), ".cacheR_test")
    if(dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
    
    # Mock getOption
    withr::with_options(list(cacheR.dir = tmp_dir), {
      res <- cacheR_default_dir()
      expect_equal(res, tmp_dir)
      expect_true(dir.exists(tmp_dir))
    })
    
    unlink(tmp_dir, recursive = TRUE)
  })
  
  test_that("cachePrune deletes old files", {
    tmp_dir <- file.path(tempdir(), "prune_test")
    dir.create(tmp_dir)
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
