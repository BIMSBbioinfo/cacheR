
# ----------------------------------------------------------------- #
test_that("cacheFile detects changes in helper functions (recursion depth 1)", {
  cache_dir <- file.path(tempdir(), "cache_recursive_1")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Create a dedicated environment
  env <- new.env()
  
  # Define helper 'g'
  env$g <- function(z) z + 1
  
  # Define main 'f' that calls 'g' directly
  # Note: environment(env$f) will be set to 'env', so it finds 'g' there.
  env$f <- function(a) {
    g(a)
  }
  
  environment(env$f) <- env
  environment(env$g) <- env
  
  # Decorate the function 'f'
  cached_f <- cacheFile(cache_dir) %@% env$f
  
  # Run 1: 10 + 1 = 11
  r1 <- cached_f(10)
  expect_equal(r1, 11)
  
  # Modify the helper 'g'
  env$g <- function(z) z + 11
  environment(env$g) <- env 
  
  # Run 2: Should detect change -> 10 + 11 = 21
  r2 <- cached_f(10)
  expect_equal(r2, 21)
})


# ----------------------------------------------------------------- #

test_that("cacheFile detects changes in global data dependencies", {
  cache_dir <- file.path(tempdir(), "cache_recursive_globals")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  env <- new.env()
  env$MY_CONFIG <- 100
  
  # Define function that uses the global variable
  f <- function() {
    return(MY_CONFIG)
  }
  environment(f) <- env 
  
  # Decorate
  cached_f <- cacheFile(cache_dir) %@% f
  
  # Run 1
  expect_equal(cached_f(), 100)
  
  # Modify global
  env$MY_CONFIG <- 999
  
  # Run 2
  expect_equal(cached_f(), 999)
})

# ----------------------------------------------------------------- #
test_that("cacheFile respects package boundaries (does not recurse into base/packages)", {
  if (exists("cacheTree_reset", mode = "function")) cacheTree_reset()
  cache_dir <- file.path(tempdir(), "cache_recursive_pkg")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir)

  f <- cacheFile(cache_dir) %@% function(x) {
    mean(x)
  }

  expect_equal(f(c(1, 2, 3)), 2)

  # Count only cache files, exclude graph.rds
  backend <- getOption("cacheR.backend", "rds")
  files <- list.files(cache_dir, pattern = paste0("\\.", backend, "$"))
  files <- files[!grepl("^graph\\.", files)]
  expect_length(files, 1)
})

# ----------------------------------------------------------------- #
test_that(".get_recursive_closure_hash captures immediate environment changes", {
  # Create an environment and a variable
  env <- new.env()
  env$x <- 10
  
  # Define function in that environment
  f <- function() {
    return(x * 2)
  }
  environment(f) <- env
  
  # Hash 1
  h1 <- .get_recursive_closure_hash(f)
  
  # Change the value in the environment
  env$x <- 20
  
  # Hash 2
  h2 <- .get_recursive_closure_hash(f)
  
  expect_false(h1 == h2)
})


# ----------------------------------------------------------------- #
test_that(".get_recursive_closure_hash captures nested dependency changes", {
  # This tests if the hasher walks down into functions called by the main function
  
  env <- new.env()
  
  # Define a helper function 'g' inside the environment
  env$g <- function(z) z + 1
  
  # Define main function 'f' that calls 'g'
  env$f <- function(a) {
    env$g(a)
  }
  
  # Set environment for f (and g implicitly shares it here, but let's be safe)
  environment(env$f) <- env
  environment(env$g) <- env
  
  # Hash 1: Base state
  h1 <- .get_recursive_closure_hash(env$f)
  
  # Modify the helper function 'g'
  # Even though 'f' didn't change, its dependency 'g' did
  env$g <- function(z) z + 100
  environment(env$g) <- env # Ensure environment is maintained if necessary
  
  # Hash 2
  h2 <- .get_recursive_closure_hash(env$f)
  
  expect_false(h1 == h2)
})

# ----------------------------------------------------------------- #
test_that(".get_recursive_closure_hash ignores irrelevant globals", {
  # OPTIONAL: If your hasher is smart enough to ignore globals not used in the function
  
  env <- new.env()
  env$used_var <- 1
  env$unused_var <- 1
  
  f <- function() used_var
  environment(f) <- env
  
  h1 <- .get_recursive_closure_hash(f)
  
  # Change variable that f does NOT use
  env$unused_var <- 999
  
  h2 <- .get_recursive_closure_hash(f)
  
  # If your recursive hasher only picks up bound variables, this should pass.
  # If it hashes the whole environment regardless of usage, expect_false.
  expect_equal(h1, h2) 
})


# ----------------------------------------------------------------- #
test_that("Cycle detection: handles mutual recursion without infinite loop", {
  # Scenario: f calls g, and g calls f.
  # A naive recursive hasher would crash R with a Stack Overflow.
  
  env <- new.env()
  
  env$f <- function(x) {
    if (x <= 0) return(0)
    env$g(x - 1)
  }
  
  env$g <- function(x) {
    env$f(x)
  }
  
  environment(env$f) <- env
  environment(env$g) <- env
  
  # Expectation: This function returns successfully (does not hang or crash)
  h1 <- .get_recursive_closure_hash(env$f)
  
  expect_true(is.character(h1))
  expect_true(nchar(h1) > 0)
  
  # Verify that changing 'g' still updates 'f's hash, even with cycles
  old_g_body <- body(env$g)
  env$g <- function(x) { x + 100 } # Break the cycle logic, but change code
  environment(env$g) <- env
  
  h2 <- .get_recursive_closure_hash(env$f)
  expect_false(h1 == h2)
})

# ----------------------------------------------------------------- #
test_that("Primitives: handles primitive functions gracefully", {
  # Primitives like `sum` or `exp` do not have standard environments
  
  f <- function(x) sum(x)
  
  # This should not fail
  h1 <- .get_recursive_closure_hash(f)
  expect_true(is.character(h1))
})

# ----------------------------------------------------------------- #
test_that("Default Arguments: dependency changes in function signature are caught", {
  env <- new.env()
  env$config_val <- 10
  
  # A helper function used in a default argument
  env$get_default <- function() env$config_val
  environment(env$get_default) <- env
  
  # Main function using that helper in its signature
  env$main_fun <- function(x = env$get_default()) {
    return(x)
  }
  environment(env$main_fun) <- env
  
  # 1. Base Hash
  h1 <- .get_recursive_closure_hash(env$main_fun)
  
  # 2. Change the dependency of the default argument
  env$config_val <- 999
  
  # 3. New Hash
  h2 <- .get_recursive_closure_hash(env$main_fun)
  
  expect_false(h1 == h2)
})

# ----------------------------------------------------------------- #
test_that("Scope isolation: Unused variables in environment do NOT affect hash", {
  # This is a specific benefit of using `codetools::findGlobals`. 
  # We don't want to invalidate cache if an unrelated variable changes.
  
  env <- new.env()
  env$used_var <- 10
  env$unused_var <- "I am noise"
  
  f <- function() {
    return(used_var + 1)
  }
  environment(f) <- env
  
  h1 <- .get_recursive_closure_hash(f)
  
  # Change the unused variable
  env$unused_var <- "I am LOUD noise"
  
  h2 <- .get_recursive_closure_hash(f)
  
  # If we are using codetools, these SHOULD be equal
  expect_equal(h1, h2)
  
  # Change the used variable
  env$used_var <- 20
  h3 <- .get_recursive_closure_hash(f)
  
  expect_false(h1 == h3)
})

# ----------------------------------------------------------------- #
test_that("Bytecode: handles compiled functions (JIT)", {
  library(compiler)
  
  f <- function(x) x + 1
  
  h1 <- .get_recursive_closure_hash(f)
  
  # Compile the function
  f_compiled <- cmpfun(f)
  
  h2 <- .get_recursive_closure_hash(f_compiled)
  
  # Ideally, a compiled function behaves the same as uncompiled,
  # but strictly speaking, the binary representation is different.
  # However, it MUST produce a valid hash and not crash.
  expect_true(is.character(h2))
  
  # Ideally, we want the hash to be identical if logic is identical, 
  # but since the body representation differs, they might differ. 
  # The critical test is that if we CHANGE the code and recompile, the hash changes.
  
  g <- function(x) x + 2
  g_compiled <- cmpfun(g)
  h3 <- .get_recursive_closure_hash(g_compiled)
  
  expect_false(h2 == h3)
})