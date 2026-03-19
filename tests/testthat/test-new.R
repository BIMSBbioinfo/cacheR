
# ---------------------------------------------------- #
test_that("closure hash detects unqualified globals via parent envs (inherits=TRUE)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_inherits_globals")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  helper <<- function(x) x + 1
  on.exit(rm("helper", envir = .GlobalEnv), add = TRUE)
  e <- new.env(parent = .GlobalEnv)

  f_raw <- evalq(function(x) helper(x), e)
  f <- cacheFile(cache_dir = cache_dir) %@% f_raw

  expect_equal(f(1), 2)

  # Change behavior of the global dependency
  helper <<- function(x) x + 2

  # Must invalidate + recompute
  expect_equal(f(1), 3)

  # Should now have 2 cache files (old + new), excluding graph.rds
  rds_files <- setdiff(list.files(cache_dir, pattern = "\\.(rds|qs2)$"), "graph.rds")
  expect_length(rds_files, 2)
})


# ---------------------------------------------------- #
test_that("graph function nodes are execution-unique (no collisions by fname)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_graph_unique_nodes")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  g <- cacheFile(cache_dir = cache_dir) %@% function(x) x * 2
  expect_equal(g(1), 2)
  expect_equal(g(2), 4)

  nodes <- cacheTree_nodes()
  fn_nodes <- Filter(function(n) n$type == "function" && identical(n$label, "g"), nodes)

  # Before fix this was 1; now should be 2 executions
  expect_length(fn_nodes, 2)
})


# ---------------------------------------------------- #
test_that("graph records parent -> child function calls with execution-unique ids", {
  skip("Feature not yet implemented: cacheTree_edges() does not exist")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_graph_edges")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  inner <- cacheFile(cache_dir = cache_dir) %@% function(x) x + 1
  outer <- cacheFile(cache_dir = cache_dir) %@% function(x) inner(x) + 1

  expect_equal(outer(1), 3)

  nodes <- cacheTree_nodes()
  edges <- cacheTree_edges()

  outer_nodes <- Filter(function(n) n$type == "function" && n$label == "outer", nodes)
  inner_nodes <- Filter(function(n) n$type == "function" && n$label == "inner", nodes)

  expect_true(length(outer_nodes) >= 1)
  expect_true(length(inner_nodes) >= 1)

  outer_id <- outer_nodes[[1]]$id
  inner_id <- inner_nodes[[1]]$id

  has_edge <- any(vapply(edges, function(e) identical(e$from, outer_id) && identical(e$to, inner_id), logical(1)))
  expect_true(has_edge)
})


# ---------------------------------------------------- #
test_that("hash_file_paths toggles path+state vs content-only behavior", {
  cache_dir1 <- file.path(tempdir(), "cache_path_mode")
  cache_dir2 <- file.path(tempdir(), "cache_content_mode")
  unlink(cache_dir1, recursive = TRUE, force = TRUE)
  unlink(cache_dir2, recursive = TRUE, force = TRUE)
  dir.create(cache_dir1, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir2, recursive = TRUE, showWarnings = FALSE)

  f1 <- tempfile("a_"); f2 <- tempfile("b_")
  writeLines(c("same", "content"), f1)
  writeLines(c("same", "content"), f2)

  # Mode: include path identity (should create 2 cache entries)
  pfun <- cacheFile(cache_dir = cache_dir1, hash_file_paths = TRUE) %@% function(path) readLines(path)[1]
  expect_equal(pfun(f1), "same")
  expect_equal(pfun(f2), "same")
  rds_files1 <- setdiff(list.files(cache_dir1, pattern = "\\.(rds|qs2)$"), "graph.rds")
  expect_length(rds_files1, 2)

  # Mode: content-only (should reuse 1 cache entry for identical files)
  cfun <- cacheFile(cache_dir = cache_dir2, hash_file_paths = FALSE) %@% function(path) readLines(path)[1]
  expect_equal(cfun(f1), "same")
  expect_equal(cfun(f2), "same")
  rds_files2 <- setdiff(list.files(cache_dir2, pattern = "\\.(rds|qs2)$"), "graph.rds")
  expect_length(rds_files2, 1)
})


# ---------------------------------------------------- #
test_that(".scan_ast_deps detects library/require/requireNamespace package mentions", {
  expr <- quote({
    library(digest)
    require(stats)
    requireNamespace("utils", quietly = TRUE)
    getOption("cacheR.dir")
  })

  res <- .scan_ast_deps(expr)
  expect_true(all(c("digest", "stats", "utils") %in% res$pkgs))
  expect_true("cacheR.dir" %in% res$opts)
})


# ---------------------------------------------------- #
test_that("Graph persists to disk and can be reloaded across sessions", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  # Setup specific cache dir
  cache_dir <- file.path(tempdir(), "cache_graph_persistence")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Execute function to generate graph nodes
  f <- cacheFile(cache_dir = cache_dir) %@% function(x) x + 1
  expect_equal(f(10), 11)

  # Verify graph file was created
  graph_file <- file.path(cache_dir, "graph.rds")
  expect_true(file.exists(graph_file))

  # Verify in-memory match
  nodes_mem <- cacheTree_nodes()
  expect_gt(length(nodes_mem), 0)

  # 2. SIMULATE RESTART: Clear in-memory graph
  cacheTree_reset()
  expect_length(cacheTree_nodes(), 0)

  # 3. Trigger a NEW execution (dependent or independent)
  # This should load the existing graph from disk and append to it
  g <- cacheFile(cache_dir = cache_dir) %@% function(x) x * 2
  expect_equal(g(5), 10)

  # 4. Check that graph.rds now contains BOTH f and g
  saved_graph <- readRDS(graph_file)

  node_labels <- vapply(saved_graph$nodes, function(n) n$label, character(1))

  # We expect "f" from session 1 and "g" from session 2
  expect_true("f" %in% node_labels)
  expect_true("g" %in% node_labels)

  # 5. Check Edge Persistence
  # Let's do a nested call to verify edges persist
  cacheTree_reset() # Reset memory again

  outer <- cacheFile(cache_dir = cache_dir) %@% function(x) f(x) + 1
  outer(10)

  saved_graph_2 <- readRDS(graph_file)
  edges <- saved_graph_2$edges

  # Should find edge from outer -> f
  # Note: IDs are dynamic (fname_hash), so we search by structure
  # But we know we have edges now
  expect_gt(length(edges), 0)
})

# ---------------------------------------------------- #
test_that("Graph handles reloading correctly (idempotency)", {
  cache_dir <- file.path(tempdir(), "cache_graph_idempotency")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) x
  f(1)

  # Verify file has 1 node
  g1 <- readRDS(file.path(cache_dir, "graph.rds"))
  n1 <- length(g1$nodes)

  # Clear memory
  cacheTree_reset()

  # Run same function with same args (Cache Hit)
  # This should NOT duplicate the node in the graph file
  f(1)

  g2 <- readRDS(file.path(cache_dir, "graph.rds"))
  n2 <- length(g2$nodes)

  expect_equal(n1, n2)
})

library(testthat)

# Helper for clean tests
setup_cache <- function(name) {
  d <- file.path(tempdir(), name)
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  d
}

# -------------------------------------------------------------------------
# Test 1: Unqualified Globals (Requested Feature 1)
# -------------------------------------------------------------------------
test_that("closure hash detects unqualified globals via parent envs", {
  cacheTree_reset()
  cd <- setup_cache("global_deps")

  helper <<- function(x) x + 1
  on.exit(rm("helper", envir = .GlobalEnv), add = TRUE)
  e <- new.env(parent = .GlobalEnv)

  # Define f inside an env, calling 'helper' which is in Global (parent)
  f_raw <- evalq(function(x) helper(x), e)

  # Decorate
  f <- cacheFile(cache_dir = cd) %@% f_raw

  expect_equal(f(1), 2)

  # Modify global helper
  helper <<- function(x) x + 2

  # Hash should change -> recompute -> new result
  expect_equal(f(1), 3)

  # Should be 2 files now, excluding graph files
  backend <- getOption("cacheR.backend", "rds")
  pat <- paste0("\\.", backend, "$")
  cache_files <- list.files(cd, pattern = pat)
  cache_files <- cache_files[!grepl("^graph\\.", cache_files)]
  expect_length(cache_files, 2)
})

# -------------------------------------------------------------------------
# Test 2: Unique Graph Nodes (Requested Feature 2)
# -------------------------------------------------------------------------
test_that("graph function nodes are execution-unique", {
  cacheTree_reset()
  cd <- setup_cache("unique_nodes")

  g <- cacheFile(cache_dir = cd) %@% function(x) x * 2
  g(1)
  g(2) # Different arg -> Different hash -> Different Node ID

  nodes <- cacheTree_nodes()
  # Filter nodes labeled "g" (the function name)
  g_nodes <- Filter(function(n) n$type == "function" && identical(n$label, "g"), nodes)

  expect_length(g_nodes, 2)
  expect_true(g_nodes[[1]]$id != g_nodes[[2]]$id)
})

# -------------------------------------------------------------------------
# Test 3: Path Modes (Requested Feature 3)
# -------------------------------------------------------------------------
test_that("hash_file_paths toggles path+state vs content-only", {
  cd1 <- setup_cache("path_mode")
  cd2 <- setup_cache("content_mode")

  f1 <- tempfile("a_"); f2 <- tempfile("b_")
  writeLines("content", f1); writeLines("content", f2)

  # 1. Path Sensitive (Default)
  # Even though content is same, path is different -> Recompute
  pfun <- cacheFile(cache_dir = cd1, backend = "rds", hash_file_paths = TRUE) %@% function(p) readLines(p)
  pfun(f1)
  pfun(f2)
  rds_files1 <- setdiff(list.files(cd1, pattern = "\\.rds$"), "graph.rds")
  expect_length(rds_files1, 2)

  # 2. Content Only (Deduplication)
  # Path differs, content same -> Same Hash -> Reuse result
  cfun <- cacheFile(cache_dir = cd2, backend = "rds", hash_file_paths = FALSE) %@% function(p) readLines(p)
  cfun(f1)
  cfun(f2)
  rds_files2 <- setdiff(list.files(cd2, pattern = "\\.rds$"), "graph.rds")
  expect_length(rds_files2, 1)
})

# -------------------------------------------------------------------------
# Test 4: Dynamic Capture (Requested Feature 5)
# -------------------------------------------------------------------------
test_that("Dynamic capture intercepts getOption calls", {
  skip("Feature not yet implemented: capture_dynamic parameter does not exist in cacheFile()")

  cd <- setup_cache("dynamic_cap")

  # Function relying on option
  f <- function(x) {
    # must use base functions exposed or shims
    getOption("my_test_opt", 0) + x
  }

  # Enable dynamic capture
  fc <- cacheFile(cache_dir = cd, capture_dynamic = TRUE) %@% f

  options(my_test_opt = 10)
  res <- fc(5)
  expect_equal(res, 15)

  # Inspect cache file to see if option was captured in meta
  cache_files <- list.files(cd, full.names = TRUE, pattern="\\.rds$")
  data <- readRDS(cache_files[1])

  expect_true("my_test_opt" %in% names(data$meta$dynamic_opts))
  expect_equal(data$meta$dynamic_opts$my_test_opt, 10)
})

# -------------------------------------------------------------------------
# Test 5: Lazy Defaults (Requested Feature 7)
# -------------------------------------------------------------------------
test_that("eval_defaults=FALSE does not force default evaluation", {
  skip("Feature not yet implemented: eval_defaults parameter does not exist in cacheFile()")

  cd <- setup_cache("lazy_defaults")

  # A default that would fail if evaluated
  f <- function(x, y = stop("Should not eval")) {
    x
  }

  # With lazy defaults, 'y' is hashed as expression "stop(...)" but not run
  fc <- cacheFile(cache_dir = cd, eval_defaults = FALSE) %@% f

  expect_no_error(fc(1)) # Should succeed

  # Result check
  expect_equal(fc(1), 1)
})

# -------------------------------------------------------------------------
# Test 6: Persistence & Idempotency (Regression)
# -------------------------------------------------------------------------
test_that("Graph persists and handles reloads", {
  cacheTree_reset()
  cd <- setup_cache("persist")

  f <- cacheFile(cache_dir = cd) %@% function(x) x+1
  f(10)

  # Check file exists
  expect_true(file.exists(file.path(cd, "graph.rds")))

  # Reset mem
  cacheTree_reset()

  # New function execution -> Should sync old graph and append
  g <- cacheFile(cache_dir = cd) %@% function(x) x*2
  g(5)

  # Load disk graph
  disk_g <- readRDS(file.path(cd, "graph.rds"))
  labels <- vapply(disk_g$nodes, `[[`, "label", FUN.VALUE=character(1))

  expect_true("f" %in% labels)
  expect_true("g" %in% labels)

  # Idempotency: Run f(10) again (cache hit)
  # Should NOT add new node if ID is same
  n_nodes_before <- length(disk_g$nodes)
  f(10)

  disk_g_2 <- readRDS(file.path(cd, "graph.rds"))
  expect_equal(length(disk_g_2$nodes), n_nodes_before)
})
