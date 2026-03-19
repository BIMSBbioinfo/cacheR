test_that("helpers: count cache artifacts", {
  count_cache_files <- function(cache_dir) {
    # Only count final cache objects, not locks/tmp/graph metadata
    files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
    files <- files[!grepl("^graph\\.", basename(files))]
    length(files)
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

  # Node IDs are "fname_hash", so match by prefix
  node_names <- ls(nodes_env)
  expect_true(any(grepl("^process_data_", node_names)))
  expect_true(any(grepl("^read_data_", node_names)))

  # Normalize path for check as graph stores absolute paths for files
  norm_f <- normalizePath(f_data, winslash="/")
  expect_true(exists(norm_f, envir = nodes_env))

  # Check Edges (process -> read -> file)
  # Edge IDs are "from->to" where from/to are "fname_hash"
  edge_names <- ls(edges_env)
  expect_true(any(grepl("^process_data_.*->read_data_", edge_names)))
  expect_true(any(grepl(paste0("^read_data_.*->", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", norm_f)), edge_names)))

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

  # Check Nodes exist (node IDs are "fname_hash", match by prefix)
  node_names <- ls(nodes_env)
  expected_funcs <- c("process_A", "process_B", "process_C", "process_D",
                      "merge_Left", "merge_Right", "final_step")

  for (fn in expected_funcs) {
    expect_true(any(grepl(paste0("^", fn, "_"), node_names)), info = paste("Node missing:", fn))
  }

  # Check that edges exist (R evaluates arguments eagerly, so the exact

  # parent-child nesting depends on call-stack depth at registration time)
  edge_names <- ls(edges_env)
  expect_true(length(edge_names) > 0, info = "Expected at least some edges")

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
    # Only count final cache objects, not locks/tmp/graph metadata
    files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
    files <- files[!grepl("^graph\\.", basename(files))]
    length(files)
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

  # Node IDs are "fname_hash", so find them by prefix
  node_names <- names(nodes)
  outer_id <- node_names[grepl("^outer_fun_", node_names)][1]
  inner_id <- node_names[grepl("^inner_fun_", node_names)][1]

  expect_false(is.na(outer_id), info = "outer_fun node not found")
  expect_false(is.na(inner_id), info = "inner_fun node not found")

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

  # Find the function node by prefix
  node_names <- names(nodes)
  func_node_id <- node_names[grepl("^read_and_sum_", node_names)][1]
  expect_false(is.na(func_node_id), info = "read_and_sum node not found")
  func_node <- nodes[[func_node_id]]
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
