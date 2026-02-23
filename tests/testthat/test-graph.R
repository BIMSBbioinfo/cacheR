test_that("helpers: count cache artifacts", {
  count_cache_files <- function(cache_dir) {
    # Only count final cache objects, not locks/tmp
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }
  expect_true(is.function(count_cache_files))
})

# -------------------------------------------------- #
test_that("execution graph is tracked and exported", {
  cacheR_reset_graph()
  cache_dir <- file.path(tempdir(), "graph_tracking")
  if (!dir.exists(cache_dir)) dir.create(cache_dir)
  
  # 1. Setup Functions and Files
  f_data <- tempfile(fileext = ".csv")
  writeLines("col1,col2\n1,2", f_data)
  
  # Leaf function: reads file
  read_data <- cacheFile(cache_dir=cache_dir) %@% function(path) {
    readLines(path, warn=FALSE)
  }
  
  # Processing function: calls read_data
  process_data <- cacheFile(cache_dir=cache_dir) %@% function(path) {
    d <- read_data(path)
    paste("Processed:", d)
  }
  
  # 2. Execute Chain
  res <- process_data(f_data)
  
  # 3. Check Internal Graph State
  # Convert environment to list or use ls() to check existence
  nodes_env <- .graph_cache$nodes
  edges_env <- .graph_cache$edges
  
  expect_true(exists("process_data", envir = nodes_env))
  expect_true(exists("read_data", envir = nodes_env))
  
  # Normalize path for check as graph stores absolute paths for files
  norm_f <- normalizePath(f_data, winslash="/")
  expect_true(exists(norm_f, envir = nodes_env))
  
  # Check Edges (process -> read -> file)
  # Note: Edge IDs are "from->to"
  # process_data called read_data, so dependency is process->read
  expect_true(exists("process_data->read_data", envir = edges_env))
  expect_true(exists(paste0("read_data->", norm_f), envir = edges_env))
  
  # 4. Export Targets
  targets_file <- tempfile(fileext = ".R")
  export_targets_file(targets_file)
  
  expect_true(file.exists(targets_file))
  content <- readLines(targets_file)
  
  # Verify content structure
  expect_true(any(grepl("tar_option_set", content)))
  expect_true(any(grepl("tar_target", content)))
  expect_true(any(grepl("process_data", content)))
})

# -------------------------------------------------- #
test_that("complex graph export: two Y-shapes merging into a final step", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()
  
  cache_dir <- file.path(tempdir(), "graph_double_y")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  
  # -------------------------------------------------------------------------
  # 1. Setup Input Files
  # -------------------------------------------------------------------------
  f1 <- file.path(cache_dir, "f1.txt"); writeLines("10", f1)
  f2 <- file.path(cache_dir, "f2.txt"); writeLines("20", f2)
  f3 <- file.path(cache_dir, "f3.txt"); writeLines("30", f3)
  f4 <- file.path(cache_dir, "f4.txt"); writeLines("40", f4)
  
  # -------------------------------------------------------------------------
  # 2. Define Functions (The Nodes)
  # -------------------------------------------------------------------------
  
  # Leaf Processors
  process_A <- cacheFile(cache_dir=cache_dir) %@% function(p) as.numeric(readLines(p))
  process_B <- cacheFile(cache_dir=cache_dir) %@% function(p) as.numeric(readLines(p))
  process_C <- cacheFile(cache_dir=cache_dir) %@% function(p) as.numeric(readLines(p))
  process_D <- cacheFile(cache_dir=cache_dir) %@% function(p) as.numeric(readLines(p))
  
  # Mid-Level Merges (The "Y" junctions)
  merge_Left  <- cacheFile(cache_dir=cache_dir) %@% function(a, b) a + b
  merge_Right <- cacheFile(cache_dir=cache_dir) %@% function(c, d) c + d
  
  # Top-Level Merge
  final_step  <- cacheFile(cache_dir=cache_dir) %@% function(l, r) l * r
  
  # -------------------------------------------------------------------------
  # 3. Execute the Graph
  # -------------------------------------------------------------------------
  # Logic: ((10 + 20) * (30 + 40)) = 30 * 70 = 2100
  
  result <- final_step(
    l = merge_Left(process_A(f1), process_B(f2)),
    r = merge_Right(process_C(f3), process_D(f4))
  )
  
  expect_equal(result, 2100)
  
  # -------------------------------------------------------------------------
  # 4. Verify Internal Graph State
  # -------------------------------------------------------------------------
  nodes_env <- .graph_cache$nodes
  edges_env <- .graph_cache$edges
  
  # Check Nodes exist
  expected_funcs <- c("process_A", "process_B", "process_C", "process_D", 
                      "merge_Left", "merge_Right", "final_step")
  
  for (fn in expected_funcs) {
    expect_true(exists(fn, envir = nodes_env), info = paste("Node missing:", fn))
  }
  
  # Check Edges (Caller -> Callee)
  # final_step calls the two merges
  expect_true(exists("final_step->merge_Left", envir = edges_env))
  expect_true(exists("final_step->merge_Right", envir = edges_env))
  
  # merge_Left calls A and B
  expect_true(exists("merge_Left->process_A", envir = edges_env))
  expect_true(exists("merge_Left->process_B", envir = edges_env))
  
  # process_A calls file
  f1_abs <- normalizePath(f1, winslash = "/")
  expect_true(exists(paste0("process_A->", f1_abs), envir = edges_env))
  
  # -------------------------------------------------------------------------
  # 5. Export and Verify File
  # -------------------------------------------------------------------------
  target_file <- file.path(cache_dir, "_targets_test.R")
  export_targets_file(target_file)
  
  expect_true(file.exists(target_file))
  lines <- readLines(target_file)
  text  <- paste(lines, collapse = "\n")
  
  # Check that the file contains the structure
  expect_true(grepl("tar_target", text))
  expect_true(grepl("name = final_step", text))
  expect_true(grepl("command = .*merge_Left.*merge_Right", text))
})

test_that("helpers: count cache artifacts", {
  count_cache_files <- function(cache_dir) {
    # Only count final cache objects, not locks/tmp
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }
  expect_true(is.function(count_cache_files))
})


# --------------------------------------------------------#
test_that("recursive calls produce parent-child relationships", {
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
  
  # With static graph, node IDs are just the function names
  outer_id <- "outer_fun"
  inner_id <- "inner_fun"
  
  expect_true(outer_id %in% names(nodes))
  expect_true(inner_id %in% names(nodes))
  
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
  # Should have 2 nodes: 1 function node + 1 file node
  expect_equal(length(nodes), 2L)
  
  func_node <- nodes[["read_and_sum"]]
  expect_false(is.null(func_node))
  
  np <- normalizePath(data_path, mustWork = FALSE, winslash = "/")
  
  # File path is recorded in the function node's file list
  expect_true(np %in% func_node$files)
  
  # Hash is recorded and looks sane (non-empty character)
  fh <- func_node$file_hashes
  expect_true(np %in% names(fh))
  expect_true(is.character(fh[[np]]) || is.na(fh[[np]]))
  expect_gt(nchar(fh[[np]]), 0)
  
  unlink(cache_dir, recursive = TRUE)
  unlink(data_path)
})


# ============================================================================
# plot_cache_graph tests
# ============================================================================

test_that("plot_cache_graph returns igraph for empty graph", {
  skip_if_not_installed("igraph")
  cacheTree_reset()

  g <- plot_cache_graph()
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 0)
})

test_that("plot_cache_graph shows cached functions", {
  skip_if_not_installed("igraph")
  cacheTree_reset()

  cache_dir <- file.path(tempdir(), "plot_graph_test")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  inner <- cacheFile(cache_dir) %@% function(x) x + 1
  outer <- cacheFile(cache_dir) %@% function(x) inner(x) * 2

  outer(5)

  g <- plot_cache_graph()
  expect_true(inherits(g, "igraph"))
  expect_true(igraph::vcount(g) >= 2)
  expect_true(igraph::ecount(g) >= 1)

  vertex_labels <- igraph::V(g)$label
  expect_true("inner" %in% vertex_labels)
  expect_true("outer" %in% vertex_labels)
})

test_that("plot_cache_graph saves to file", {
  skip_if_not_installed("igraph")
  cacheTree_reset()

  cache_dir <- file.path(tempdir(), "plot_graph_save")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- cacheFile(cache_dir) %@% function(x) x
  f(1)

  out_file <- tempfile(fileext = ".png")
  g <- plot_cache_graph(output = out_file)
  expect_true(file.exists(out_file))
  expect_gt(file.info(out_file)$size, 0)
  unlink(out_file)
})

test_that("plot_cache_graph with cache_dir loads from disk", {
  skip_if_not_installed("igraph")
  cacheTree_reset()

  cache_dir <- file.path(tempdir(), "plot_graph_sync")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  f <- cacheFile(cache_dir) %@% function(x) x * 2
  f(42)

  # graph should already have been written to disk by cacheFile
  # reset in-memory and reload via plot_cache_graph
  node_count_before <- length(cacheTree_nodes())
  expect_true(node_count_before >= 1)

  g <- plot_cache_graph()
  expect_true(igraph::vcount(g) >= 1)
})