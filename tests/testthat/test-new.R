test_that("helpers: count cache artifacts", {
  count_cache_files <- function(cache_dir) {
    # Only count final cache objects, not locks/tmp
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }
  expect_true(is.function(count_cache_files))
})

# -------------------------------------------------- #
test_that("file paths in global config list invalidate cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_config_list_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  config <<- list(path = tempfile(fileext = ".txt"))
  on.exit(rm(config, envir = .GlobalEnv), add = TRUE)

  writeLines("A", config$path)

  f <- cacheFile(cache_dir = cache_dir) %@% function() {
    readLines(config$path, warn = FALSE)
  }

  r1 <- f()
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  writeLines("B", config$path)

  # Desired behaviour: invalidation because config$path points to changed file
  r2 <- f()
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})

# -------------------------------------------------- #
test_that("file paths nested inside argument lists invalidate cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_arg_list_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  cfg <- list(path = tempfile(fileext = ".txt"))
  writeLines("A", cfg$path)

  g <- cacheFile(cache_dir = cache_dir) %@% function(cfg) {
    readLines(cfg$path, warn = FALSE)
  }

  r1 <- g(cfg)
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  writeLines("B", cfg$path)

  # Desired behaviour: invalidation because cfg$path file changed
  r2 <- g(cfg)
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})

# -------------------------------------------------- #
test_that("changes in options used by function invalidate cache (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_options_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  old_opt <- getOption("cacheR.test.multiplier", NULL)
  on.exit(options(cacheR.test.multiplier = old_opt), add = TRUE)

  options(cacheR.test.multiplier = 1)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) {
    x * getOption("cacheR.test.multiplier")
  }

  r1 <- f(10)
  expect_equal(r1, 10)
  expect_equal(count_cache_files(cache_dir), 1L)

  options(cacheR.test.multiplier = 2)

  # Desired behaviour: invalidation because option changed
  r2 <- f(10)
  expect_equal(r2, 20)
  expect_equal(count_cache_files(cache_dir), 2L)
})

# -------------------------------------------------- #
test_that("non-character file reference via connection invalidates cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_nonchar_connection_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  p <- tempfile(fileext = ".txt")
  writeLines("A", p)

  con <- file(p, open = "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    seek(con, 0, rw = "read")
    readLines(con, warn = FALSE)
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  writeLines("B", p)

  # Desired: invalidation because underlying file changed (even though arg is a connection)
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})

# -------------------------------------------------- #
test_that("DBI connection data changes invalidate cache (no runs counter)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_dbi_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE t(val TEXT)")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('A')")

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    DBI::dbGetQuery(con, "SELECT val FROM t LIMIT 1")$val[[1]]
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  DBI::dbExecute(con, "DELETE FROM t")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('B')")

  # Desired: invalidation because DB contents changed
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})


# -------------------------------------------------- #
test_that("non-character file reference via connection invalidates cache when file changes (no runs counter)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_nonchar_connection_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  p <- tempfile(fileext = ".txt")
  writeLines("A", p)

  con <- file(p, open = "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    seek(con, 0, rw = "read")
    readLines(con, warn = FALSE)
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  writeLines("B", p)

  # Desired: invalidation because underlying file changed (even though arg is a connection)
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})

# -------------------------------------------------- #
test_that("DBI connection data changes invalidate cache (no runs counter)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_dbi_no_runs")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  count_cache_files <- function(cache_dir) {
    length(list.files(cache_dir, pattern = "\\.(rds|qs)$", full.names = TRUE))
  }

  db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE t(val TEXT)")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('A')")

  f <- cacheFile(cache_dir = cache_dir) %@% function(con) {
    DBI::dbGetQuery(con, "SELECT val FROM t LIMIT 1")$val[[1]]
  }

  r1 <- f(con)
  expect_equal(r1, "A")
  expect_equal(count_cache_files(cache_dir), 1L)

  DBI::dbExecute(con, "DELETE FROM t")
  DBI::dbExecute(con, "INSERT INTO t(val) VALUES ('B')")

  # Desired: invalidation because DB contents changed
  r2 <- f(con)
  expect_equal(r2, "B")
  expect_equal(count_cache_files(cache_dir), 2L)
})
