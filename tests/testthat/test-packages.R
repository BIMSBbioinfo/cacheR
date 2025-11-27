test_that("cacheFile stores pkgs metadata when function uses non-base package", {
  skip_if_not_installed("digest")

  # optional: reset cache tree if you have this helper
  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_pkgdeps1")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) {
    # use a non-base function so .find_package_deps picks up 'digest'
    digest::digest(x)
  }

  res <- f(1)  # trigger caching

  files <- list.files(cache_dir, full.names = TRUE)
  expect_length(files, 1)

  # read cache metadata
  obj <- .cacheR_read(files[1])

  # we should have a 'pkgs' entry with 'digest' in it
  expect_true("pkgs" %in% names(obj))
  expect_s3_class(obj$pkgs, "data.frame")
  expect_true("package" %in% names(obj$pkgs))
  expect_true("version" %in% names(obj$pkgs))
  expect_true("digest" %in% obj$pkgs$package)

  # second call should hit the cache, not create new file
  res2 <- f(1)
  files2 <- list.files(cache_dir, full.names = TRUE)
  expect_equal(length(files2), 1)
})

test_that("cacheFile hash changes when pkgs metadata changes", {
  # This does not depend on .find_package_deps, we just mutate the .pkg_deps
  # default argument on the wrapper function and check that we get a
  # different args_hash (via filename) for the same call.

  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_pkgdeps2")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  cf <- cacheFile(cache_dir = cache_dir) %@% function(x) {
    x + 1
  }

  # First call with original .pkg_deps (likely NULL, as only base functions used)
  invisible(cf(1, .load = FALSE))

  files1 <- list.files(cache_dir)
  expect_length(files1, 1)

  # Extract hash from filename: <fname>.<hash>.rds
  hash_from_name <- function(fn) {
    sub("^[^.]+\\.([^.]+)\\.rds$", "\\1", fn)
  }
  hash1 <- hash_from_name(files1[1])

  # Now mutate the default .pkg_deps on the wrapper
  fake_pkgs <- data.frame(
    package = "fakepkg",
    version = "1.0.0",
    stringsAsFactors = FALSE
  )
  fml <- formals(cf)
  fml$.pkg_deps <- fake_pkgs
  formals(cf) <- fml

  # Second call with same arguments but different .pkg_deps
  invisible(cf(1, .load = FALSE))

  files2 <- list.files(cache_dir)
  # We should now have 2 different cache files
  expect_true(length(files2) >= 2)

  hashes <- vapply(files2, hash_from_name, character(1))
  expect_true(length(unique(hashes)) >= 2)
  expect_false(hash1 %in% hashes[-match(files1[1], files2)])

  # Sanity: both files should be readable and contain pkgs in metadata
  objs <- lapply(file.path(cache_dir, files2), .cacheR_read)
  expect_true(all(vapply(objs, function(o) "pkgs" %in% names(o), logical(1))))
})

test_that("cacheFile pkgs metadata is NULL for base-only functions", {
  if (exists("cacheTree_reset", mode = "function"))
    cacheTree_reset()

  cache_dir <- file.path(tempdir(), "cache_pkgdeps3")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  f <- cacheFile(cache_dir = cache_dir) %@% function(x) {
    # only base functions
    sum(x) + length(x)
  }

  invisible(f(1:3))

  files <- list.files(cache_dir, full.names = TRUE)
  expect_length(files, 1)

  obj <- .cacheR_read(files[1])

  # We do expect the 'pkgs' field to exist, but be NULL
  expect_true("pkgs" %in% names(obj))
  expect_true(is.null(obj$pkgs) || nrow(obj$pkgs) == 0)
})