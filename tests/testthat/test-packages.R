library(testthat)
library(mockery)

test_that("cache invalidates when a dependent package version changes", {
  
  # 1. Setup a dummy function
  f <- function(x) x 
  
  # 2. TRICK: Force 'f' to pretend it belongs to the 'stats' package namespace.
  # This ensures the hasher sees it as a package function immediately, 
  # triggering the version check at the top level (where the mock exists),
  # rather than recursing into un-mocked territory.
  environment(f) <- asNamespace("stats")
  
  # 3. First run: Mock version 1.0.0
  m_ver_1 <- mock(package_version("1.0.0"))
  
  # Note: stub() modifies the function's environment for the scope of the test.
  # We stub 'utils::packageVersion' inside your hasher.
  stub(.get_recursive_closure_hash, "utils::packageVersion", m_ver_1)
  
  hash_v1 <- .get_recursive_closure_hash(f)
  
  # 4. Second run: Mock version 1.1.0
  # We re-apply the stub with the new mock value
  m_ver_2 <- mock(package_version("1.1.0"))
  stub(.get_recursive_closure_hash, "utils::packageVersion", m_ver_2)
  
  hash_v2 <- .get_recursive_closure_hash(f)
  
  # 5. Expect the hashes to be different
  expect_false(hash_v1 == hash_v2)
})