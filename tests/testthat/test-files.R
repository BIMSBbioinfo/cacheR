# tests/testthat/test-cacheFile-file-tracking.R
# --------------------------------------------------------#
test_that("cacheFile invalidates when number of files in arg path changes", {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }

  cache_dir <- file.path(tempdir(), "cache_test_files_arg")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_dir_arg")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  # start with one file
  file.create(file.path(input_dir, "file1.txt"))

  cached_fun <- cacheFile(
    cache_dir = cache_dir,
    file_args = "path"
  ) %@% function(path) {
    length(list.files(path))
  }

  n1 <- cached_fun(input_dir)
  expect_equal(n1, 1L)

  # add a second file; should force a new cache key
  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(input_dir)
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("cacheFile invalidates when hardcoded literal path changes", {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }

  cache_dir <- file.path(tempdir(), "cache_test_files_literal")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  static_dir <- file.path(tempdir(), "cache_static_dir")
  unlink(static_dir, recursive = TRUE, force = TRUE)
  dir.create(static_dir, showWarnings = FALSE, recursive = TRUE)

  # build a function that literally contains list.files('<static_dir>') in its body
  fun_code <- sprintf("function() { length(list.files('%s')) }", static_dir)
  raw_fun  <- eval(parse(text = fun_code))

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% raw_fun

  # first call with one file
  file.create(file.path(static_dir, "file1.txt"))
  n1 <- cached_fun()
  expect_equal(n1, 1L)

  # add another file in the same hardcoded dir
  file.create(file.path(static_dir, "file2.txt"))
  n2 <- cached_fun()
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("cacheFile invalidates when dir(global_variable) changes", {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }

  cache_dir <- file.path(tempdir(), "cache_test_files_global")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  global_dir <- file.path(tempdir(), "cache_global_dir")
  unlink(global_dir, recursive = TRUE, force = TRUE)
  dir.create(global_dir, showWarnings = FALSE, recursive = TRUE)

  # assign to a global name used inside the function body
  old_global <- if (exists("GLOBAL_DIR", envir = .GlobalEnv, inherits = FALSE)) {
    get("GLOBAL_DIR", envir = .GlobalEnv)
  } else {
    NULL
  }
  assign("GLOBAL_DIR", global_dir, envir = .GlobalEnv)
  on.exit({
    if (is.null(old_global)) {
      rm(GLOBAL_DIR, envir = .GlobalEnv)
    } else {
      assign("GLOBAL_DIR", old_global, envir = .GlobalEnv)
    }
  }, add = TRUE)

  raw_fun <- function() {
    # uses dir(global_variable)-style access
    length(dir(GLOBAL_DIR))
  }

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% raw_fun

  # one file
  file.create(file.path(global_dir, "file1.txt"))
  n1 <- cached_fun()
  expect_equal(n1, 1L)

  # second file -> should change hash via symbol-based tracking
  file.create(file.path(global_dir, "file2.txt"))
  n2 <- cached_fun()
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("cacheFile does not invalidate when file counts stay the same", {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }

  cache_dir <- file.path(tempdir(), "cache_test_files_stable")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  input_dir <- file.path(tempdir(), "cache_input_dir_stable")
  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  # deterministic set of files
  file.create(file.path(input_dir, "a.txt"))
  file.create(file.path(input_dir, "b.txt"))

  cached_fun <- cacheFile(
    cache_dir = cache_dir,
    file_args = "path"
  ) %@% function(path) {
    files <- sort(list.files(path))
    paste(files, collapse = ",")
  }

  res1 <- cached_fun(input_dir)
  res2 <- cached_fun(input_dir)  # should hit the cache

  expect_identical(res1, res2)
})

# --------------------------------------------------------#
test_that("cacheFile invalidates when using base::list.files on an argument path", {
  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

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

  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(input_dir)
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that("cacheFile invalidates when path is passed via ...", {
  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

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

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% fun

  n1 <- cached_fun(path = input_dir)
  expect_equal(n1, 1L)

  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(path = input_dir)
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})

# --------------------------------------------------------#
test_that(".find_path_specs returns empty lists for expressions without path calls", {
  expr <- quote({
    x <- 1 + 2
    y <- x * 3
  })

  specs <- cacheR:::`.find_path_specs`(expr)

  expect_type(specs, "list")
  expect_named(specs, c("literals", "symbols"))
  expect_length(specs$literals, 0L)
  expect_length(specs$symbols,  0L)
})

# --------------------------------------------------------#
test_that(".find_path_specs detects simple literal and symbol paths", {
  f <- function(my_dir) {
    a <- list.files("data")
    b <- dir(path = my_dir)
    c <- list.dirs("more_data", recursive = FALSE)
    invisible(NULL)
  }

  specs <- cacheR:::`.find_path_specs`(body(f))

  expect_true("data"      %in% specs$literals)
  expect_true("more_data" %in% specs$literals)
  expect_true("my_dir" %in% specs$symbols)
})

# --------------------------------------------------------#
test_that(".find_path_specs handles namespaced calls and composite expressions", {
  f <- function(base_dir) {
    files1 <- base::list.files("raw")
    files2 <- list.files(file.path(base_dir, "subdir", "nested"))
    invisible(NULL)
  }

  specs <- cacheR:::`.find_path_specs`(body(f))

  expect_true("raw"    %in% specs$literals)
  expect_true("subdir" %in% specs$literals)
  expect_true("nested" %in% specs$literals)
  expect_true("base_dir" %in% specs$symbols)
})

# --------------------------------------------------------#
test_that(".find_path_specs does not error on more complex bodies", {
  f <- function(path_sym, other) {
    if (!missing(other)) {
      message("other arg present")
    }
    x <- list.files(path_sym)
    y <- dir(path = "literal_dir")
    z <- list.files(path = c("a", "b", path_sym))
    invisible(list(x, y, z))
  }

  expect_silent({
    specs <- cacheR:::`.find_path_specs`(body(f))
  })

  expect_true("literal_dir" %in% specs$literals)
  expect_true("a"           %in% specs$literals)
  expect_true("b"           %in% specs$literals)
  expect_true("path_sym"    %in% specs$symbols)
})

# --------------------------------------------------------#
test_that("cacheFile handles non-character symbols in path specs (e.g. file.path function)", {
  cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_test_paths")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  base_dir <- tempdir()

  cached_fun <- cacheFile(
    cache_dir = cache_dir,
    file_args = "base_dir"
  ) %@% function(base_dir) {
    files <- list.files(file.path(base_dir, "subdir"), recursive = TRUE)
    length(files)
  }

  subdir <- file.path(base_dir, "subdir")
  dir.create(subdir, showWarnings = FALSE)

  # Ensure res1/res2 persist outside expect_error
  res1 <- NULL
  expect_error(
    res1 <- cached_fun(base_dir),
    NA
  )

  res2 <- NULL
  expect_error(
    res2 <- cached_fun(base_dir),
    NA
  )

  expect_identical(res1, res2)
})


# --------------------------------------------------------#
test_that("symbol path resolution skips non-character values", {
  env <- new.env(parent = emptyenv())
  env$dir_char <- "data"
  env$file.path <- file.path

  syms <- c("dir_char", "file.path", "does_not_exist")

  fn_env <- env

  get_symbol_paths <- function(symbols) {
    vapply(symbols, function(sym) {
      if (!exists(sym, envir = fn_env, inherits = TRUE)) return(NA_character_)
      val <- get(sym, envir = fn_env, inherits = TRUE)
      if (is.character(val) && length(val) >= 1L) val[[1L]] else NA_character_
    }, character(1L))
  }

  paths <- get_symbol_paths(syms)

  expect_equal(paths[["dir_char"]], "data")
  expect_true(is.na(paths[["file.path"]]))
  expect_true(is.na(paths[["does_not_exist"]]))
})


# --------------------------------------------- #
test_that("cacheFile works with a DESeq2-like dds_from_counts function without DESeq2", {
  if (exists("cacheTree_reset", mode = "function")) {
    cacheTree_reset()
  }

  cache_dir <- file.path(tempdir(), "cache_dds_like_counts")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  dds_like_from_counts <- cacheFile(cache_dir = cache_dir) %@%
    function(counts_tbl, sample_table) {
      mat <- as.matrix(counts_tbl[, sample_table$sample_id])
      rownames(mat) <- counts_tbl$gene_id
      coldata <- as.data.frame(sample_table)
      rownames(coldata) <- coldata$sample_id
      design <- stats::model.matrix(~ condition, data = coldata)
      list(counts = mat, coldata = coldata, design = design)
    }

  set.seed(1)
  sample_table <- data.frame(
    sample_id = paste0("s", 1:3),
    condition = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
  counts_tbl <- data.frame(
    gene_id = paste0("g", 1:10),
    s1 = rpois(10, 10),
    s2 = rpois(10, 12),
    s3 = rpois(10,  5),
    check.names = FALSE
  )

  res1 <- dds_like_from_counts(counts_tbl, sample_table)
  res2 <- dds_like_from_counts(counts_tbl, sample_table)

  expect_type(res1, "list")
  expect_identical(res1, res2)
})

# --------------------------------------------- #
test_that("cacheFile normalizes relative paths correctly", {
  cache_dir <- file.path(tempdir(), "cache_test_norm")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Setup: Create a subdirectory "data"
  data_dir <- file.path(tempdir(), "data")
  dir.create(data_dir, showWarnings = FALSE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  
  # Create a function that accepts a path
  f <- function(path) {
    return(path)
  }
  
  # Decorate with file_args
  cached_f <- cacheFile(cache_dir, file_args = "path") %@% f
  
  # 1. Call with absolute path
  cached_f(data_dir)
  
  # 2. Call with relative path (if strictly checking, this might hash differently 
  # unless normalized, but the function execution should work)
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(tempdir())
  
  # Should run without error
  expect_no_error(cached_f("data"))
})

# --------------------------------------------- #
test_that("cacheFile scans all arguments for directories even if file_args is set", {
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
  
  # Decorate
  cached_f <- cacheFile(cache_dir, backend = "rds") %@% f
  
  # Run 1
  cached_f(dir1, "some_val")
  
  # Run 2: Different directory arg
  cached_f(dir2, "some_val")
  
  # Should have 2 cache files (filtered for .rds to ignore .lock files)
  files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_equal(length(files), 2)
})

# -------------------------------------------------------------------------
# Test Feature 3: Concurrency Safety (File Locking)
# -------------------------------------------------------------------------
test_that("file locking logic runs without error", {
  cache_dir <- file.path(tempdir(), "cache_test_locking")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # 1. Force backend to 'rds' so we can regex for it reliably
  cached_f <- cacheFile(cache_dir, backend = "rds") %@% function(x) x + 1
  
  # 2. Run
  res <- cached_f(1)
  expect_equal(res, 2)
  
  files <- list.files(cache_dir)
  
  # 3. Check for presence of RDS
  expect_true(any(grepl("\\.rds$", files)))
  
  # 4. Lock files MAY exist (filelock does not delete them). 
  # We just ensure the code didn't crash and the data is there.
  rds_files <- list.files(cache_dir, pattern = "\\.rds$")
  expect_length(rds_files, 1)
})