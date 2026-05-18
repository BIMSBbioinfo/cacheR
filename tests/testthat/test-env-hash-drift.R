# Regression tests for env-hash drift.
#
# Observed 2026-05-17 in a Seurat-heavy pipeline: a no-arg deterministic
# function wrapped by cacheFile() produced multiple cache files with
# different env_hash values across two calls in one R session. body_hash
# was stable. Drift was inside `.get_scoped_env_hash`, traced to two
# structural issues:
#
#   1. captured helper functions were hashed via body()/formals() with the
#      `srcref` attribute attached. srcref points to a `srcfile` whose
#      `parseData` env is mutated by R as more code is parsed in the
#      session, so the serialized bytes drift between calls.
#
#   2. non-function globals returned by codetools::findGlobals were
#      captured by value via `inherits = TRUE`, which silently pulled in
#      package-namespace objects (S4 generics, dispatch tables, etc.)
#      whose internal state mutates between calls.
#
# These tests pin the invariants we need to hold.

setup_cache <- function(name) {
  d <- file.path(tempdir(), name)
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# --------------------------------------------------------------------- #
test_that(".get_scoped_env_hash is stable across repeated calls in one session", {
  env <- new.env(parent = globalenv())
  env$inner1 <- function(x) x + 1
  env$inner2 <- function(x) x * 2
  env$middle <- function(x) inner1(x) + inner2(x)
  env$outer  <- function() middle(10)
  for (nm in c("inner1", "inner2", "middle", "outer")) {
    environment(env[[nm]]) <- env
  }

  h1 <- cacheR:::.get_scoped_env_hash(env$outer)

  # Simulate session activity that perturbs the parser's srcfile cache.
  for (i in seq_len(50)) {
    eval(parse(text = sprintf("function(x) x + %d", i), keep.source = TRUE))
  }
  invisible(gc())

  h2 <- cacheR:::.get_scoped_env_hash(env$outer)
  expect_identical(h1, h2)
})

# --------------------------------------------------------------------- #
test_that(".get_scoped_env_hash is invariant to srcref attribute on helpers", {
  # Helpers parsed WITH keep.source carry srcref attributes that reference
  # a mutable srcfile environment. The hash must not depend on them.
  env_a <- new.env(parent = globalenv())
  eval(parse(text = "h <- function(x) x + 1\nf <- function() h(10)",
             keep.source = TRUE), envir = env_a)
  environment(env_a$h) <- env_a
  environment(env_a$f) <- env_a

  # Equivalent helpers with srcref attributes stripped.
  env_b <- new.env(parent = globalenv())
  env_b$h <- function(x) x + 1
  env_b$f <- function() h(10)
  env_b$h <- removeSource(env_b$h)
  env_b$f <- removeSource(env_b$f)
  environment(env_b$h) <- env_b
  environment(env_b$f) <- env_b

  h_a <- cacheR:::.get_scoped_env_hash(env_a$f)
  h_b <- cacheR:::.get_scoped_env_hash(env_b$f)
  expect_identical(h_a, h_b)
})

# --------------------------------------------------------------------- #
test_that(".get_scoped_env_hash is invariant to srcref mutation on bodies", {
  # Directly mutate the srcref attribute on a helper between two calls,
  # simulating what happens when R re-parses code into a new srcfile.
  env <- new.env(parent = globalenv())
  eval(parse(text = "h <- function(x) x + 1\nf <- function() h(10)",
             keep.source = TRUE), envir = env)
  environment(env$h) <- env
  environment(env$f) <- env

  h1 <- cacheR:::.get_scoped_env_hash(env$f)

  # Replace the helper body with an equivalent expression carrying a
  # different srcref (different srcfile, identical text).
  fresh <- eval(parse(text = "function(x) x + 1", keep.source = TRUE))
  body(env$h) <- body(fresh)

  h2 <- cacheR:::.get_scoped_env_hash(env$f)
  expect_identical(h1, h2)
})

# --------------------------------------------------------------------- #
test_that("End-to-end: deep dep tree yields exactly one cache file across calls", {
  cache_dir <- setup_cache("envdrift_deep")
  on.exit(unlink(cache_dir, recursive = TRUE))

  env <- new.env(parent = globalenv())
  eval(parse(text = paste(
    "s1 <- function(x) x + 1",
    "s2 <- function(x) s1(x) * 2",
    "s3 <- function(x) s2(x) - 1",
    "s4 <- function(x) s3(x) + s1(x)",
    "s5 <- function(x) s4(x) * s2(x)",
    "s6 <- function(x) s5(x) + s3(x)",
    "s7 <- function(x) s6(x) - s4(x)",
    "orchestrator <- function() s7(10)",
    sep = "\n"
  ), keep.source = TRUE), envir = env)
  for (nm in ls(env)) environment(env[[nm]]) <- env

  cf <- cacheFile(cache_dir = cache_dir) %@% env$orchestrator

  r1 <- cf()
  # Simulate the session activity that triggered the production bug:
  # parse and evaluate unrelated code that grows R's parser cache.
  for (i in seq_len(20)) {
    eval(parse(text = sprintf("dummy_%d <- function() %d", i, i),
               keep.source = TRUE),
         envir = globalenv())
  }
  on.exit({
    for (i in seq_len(20)) {
      if (exists(sprintf("dummy_%d", i), envir = globalenv())) {
        rm(list = sprintf("dummy_%d", i), envir = globalenv())
      }
    }
  }, add = TRUE)
  r2 <- cf()

  expect_identical(r1, r2)

  # Match both backends — .onLoad picks qs2 when installed, otherwise rds.
  files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$")
  result_files <- files[!grepl("(graph|computing|lock|tmp)", files)]
  expect_length(result_files, 1)
})

# --------------------------------------------------------------------- #
test_that("Captured non-function globals from package namespaces do not drift", {
  # codetools::findGlobals can return names that resolve via inherits=TRUE
  # into a package namespace (S4 generics, package constants). Those
  # values should not be captured by value, because their internal state
  # is session-dependent.
  env <- new.env(parent = globalenv())
  # `.Machine` is a base constant — stands in for any package-bound value.
  env$f <- function() .Machine$double.eps

  h1 <- cacheR:::.get_scoped_env_hash(env$f)
  # Trigger something that could mutate package-internal state (loading
  # methods, growing dispatch tables). `gc()` and namespace touches are
  # enough to perturb session caches in the production case.
  invisible(getNamespace("utils"))
  invisible(gc())
  h2 <- cacheR:::.get_scoped_env_hash(env$f)

  expect_identical(h1, h2)
})

# --------------------------------------------------------------------- #
test_that("User-defined non-function globals are still tracked (no regression)", {
  # Counterpart to the previous test: legitimate user globals bound in a
  # user-controlled env must still invalidate the cache when changed.
  cache_dir <- setup_cache("envdrift_user_globals")
  on.exit(unlink(cache_dir, recursive = TRUE))

  env <- new.env(parent = globalenv())
  env$MY_CONFIG <- 100
  env$f <- function() MY_CONFIG
  environment(env$f) <- env

  cached_f <- cacheFile(cache_dir) %@% env$f
  expect_equal(cached_f(), 100)

  env$MY_CONFIG <- 999
  expect_equal(cached_f(), 999)
})

# --------------------------------------------------------------------- #
test_that("track_env = FALSE skips the recursive scoped-env walk", {
  cache_dir <- setup_cache("envdrift_track_off")
  on.exit(unlink(cache_dir, recursive = TRUE))

  env <- new.env(parent = globalenv())
  env$helper <- function(x) x + 1
  env$f      <- function() helper(1)
  environment(env$helper) <- env
  environment(env$f)      <- env

  cached_f <- cacheFile(cache_dir, track_env = FALSE) %@% env$f
  r1 <- cached_f()
  expect_equal(r1, 2)

  # Silently change the helper. With track_env = FALSE the cache key
  # only sees the outer body+args, so we expect a cache HIT (stale value).
  env$helper <- function(x) x + 99
  environment(env$helper) <- env
  r2 <- cached_f()
  expect_equal(r2, 2)  # stale by design

  # Match both backends — .onLoad picks qs2 when installed, otherwise rds.
  files <- list.files(cache_dir, pattern = "\\.(rds|qs2)$")
  result_files <- files[!grepl("(graph|computing|lock|tmp)", files)]
  expect_length(result_files, 1)
})

# --------------------------------------------------------------------- #
test_that("track_env = TRUE (default) still invalidates on helper change", {
  cache_dir <- setup_cache("envdrift_track_on")
  on.exit(unlink(cache_dir, recursive = TRUE))

  env <- new.env(parent = globalenv())
  env$helper <- function(x) x + 1
  env$f      <- function() helper(1)
  environment(env$helper) <- env
  environment(env$f)      <- env

  cached_f <- cacheFile(cache_dir) %@% env$f
  expect_equal(cached_f(), 2)

  env$helper <- function(x) x + 99
  environment(env$helper) <- env
  expect_equal(cached_f(), 100)
})
