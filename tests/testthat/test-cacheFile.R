# --------------------------------------------------------#
test_that("cacheFile caches results and avoids re-running", {
  cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "cache_test_1")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  runs <- 0
  
  cached_fun <- cacheFile(cache_dir) %@% function(x) {
    runs <<- runs + 1
    x * 2
  }
  
  # First call: should run the body
  expect_equal(cached_fun(10), 20)
  expect_equal(runs, 1L)
  
  # Second call with same args: should load from cache; runs should not change
  expect_equal(cached_fun(10), 20)
  expect_equal(runs, 1L)
  
  # Different args => new run
  expect_equal(cached_fun(5), 10)
  expect_equal(runs, 2L)
  
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
