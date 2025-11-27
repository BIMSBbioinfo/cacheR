# tests/testthat/test-cacheFile-file-tracking.R
# --------------------------------------------------------#
test_that("cacheFile invalidates when number of files in arg path changes", {
  # optional, depending on your package
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
    # mimics a function that internally uses list.files(path)
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
    # do something slightly more than just counting
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
    # now tracked via .find_path_specs, even though namespaced
    length(base::list.files(path))
  }

  n1 <- cached_fun(input_dir)
  expect_equal(n1, 1L)

  # add a file; hash should change and function should recompute
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

  # start with one file
  file.create(file.path(input_dir, "file1.txt"))

  fun <- function(...) {
    args <- list(...)
    path <- args$path
    length(list.files(path))
  }

  cached_fun <- cacheFile(cache_dir = cache_dir) %@% fun

  n1 <- cached_fun(path = input_dir)
  expect_equal(n1, 1L)

  # add a second file; should force a new cache key
  file.create(file.path(input_dir, "file2.txt"))

  n2 <- cached_fun(path = input_dir)
  expect_equal(n2, 2L)
  expect_gt(n2, n1)
})


