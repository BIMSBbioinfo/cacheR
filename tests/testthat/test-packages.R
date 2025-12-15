library(testthat)
library(mockery)
# --------------------------------------------------------#
test_that("cache invalidates when a dependent package version changes", {
  
  # 1. Setup a dummy function
  f <- function(x) x 
  
  # Force 'f' to pretend it belongs to the 'stats' package namespace.
  environment(f) <- asNamespace("stats")
  
  # 2. Run 1: Simulate Version 1.0.0
  # We inject a checker that claims 'stats' is version 1.0.0
  hash_v1 <- .get_recursive_closure_hash(f, version_checker = function(pkg) {
    return("1.0.0") 
  })
  
  # 3. Run 2: Simulate Version 1.1.0
  # We inject a checker that claims 'stats' is version 1.1.0
  hash_v2 <- .get_recursive_closure_hash(f, version_checker = function(pkg) {
    return("1.1.0")
  })
  
  # 4. Expect the hashes to be different
  expect_false(hash_v1 == hash_v2)
})


# --------------------------------------------------------#

test_that("version checker is propagated recursively to inner functions", {
  
  # Inner function (Package function)
  inner <- function(x) x
  environment(inner) <- asNamespace("stats")
  
  # Outer function (User function)
  outer <- function() {
    inner(1)
  }
  
  # Run 1: Inner function is v1.0.0
  h1 <- .get_recursive_closure_hash(outer, version_checker = function(p) "1.0.0")
  
  # Run 2: Inner function is v1.1.0
  h2 <- .get_recursive_closure_hash(outer, version_checker = function(p) "1.1.0")
  
  # If propagation works, 'h2' will be different because 'inner' changed versions.
  # If propagation FAILED, 'h2' would equal 'h1' (because it would have fallen back 
  # to the real installed package version for both runs).
  expect_false(h1 == h2)
})