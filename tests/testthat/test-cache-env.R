setup_cache <- function(name) {
  d <- file.path(tempdir(), name)
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d, showWarnings = FALSE)
  d
}

# --------------------------------------------------------#
test_that("Environment Tracking: Modifying a variable inside a global environment invalidates cache", {
  cache_dir <- setup_cache("env_basic")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # 1. Setup Global Environment Object
  # The hasher detects this is an environment, converts it to a list, 
  # and hashes the content.
  config <- new.env(parent = emptyenv())
  config$threshold <- 10
  
  # Function depends on 'config' (global variable)
  proc <- cacheFile(cache_dir, backend="rds") %@% function(x) {
    x * config$threshold
  }
  
  # Run 1: 5 * 10 = 50
  expect_equal(proc(5), 50)
  
  # 2. Modify content inside the environment
  # The environment *reference* (memory address) hasn't changed, 
  # but the recursive hasher sees the value inside changed.
  config$threshold <- 20
  
  # Run 2: Should INVALIDATE and re-run -> 5 * 20 = 100
  expect_equal(proc(5), 100)
  
  # Verify distinct cache files exist
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 2)
})

# --------------------------------------------------------#
test_that("Environment Tracking: Detects changes in Nested Environments", {
  cache_dir <- setup_cache("env_nested")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Setup Nested Structure: outer -> inner -> value
  settings <- new.env(parent = emptyenv())
  settings$inner <- new.env(parent = emptyenv())
  settings$inner$val <- 100
  
  proc <- cacheFile(cache_dir, backend="rds") %@% function() {
    return(settings$inner$val)
  }
  
  # Run 1
  expect_equal(proc(), 100)
  
  # Modify deep value
  settings$inner$val <- 999
  
  # Run 2: Should detect change deep in the structure
  expect_equal(proc(), 999)
})

# --------------------------------------------------------#
test_that("Environment Tracking: Detects files defined inside environments", {
  # This tests the "Scan Everything" logic applied recursively inside environments
  cache_dir <- setup_cache("env_files")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  data_container <- new.env(parent = emptyenv())
  
  # Create a dummy file
  f_path <- file.path(tempdir(), "env_file.txt")
  writeLines("version1", f_path)
  on.exit(unlink(f_path), add = TRUE)
  
  # Point the environment variable to the file
  data_container$path <- f_path
  
  proc <- cacheFile(cache_dir, backend="rds") %@% function() {
    readLines(data_container$path)
  }
  
  # Run 1
  expect_equal(proc(), "version1")
  
  # Modify the FILE content (not the variable string)
  Sys.sleep(1.1)
  writeLines("version2", f_path)
  
  # Run 2: 
  # Hasher sees 'data_container' -> 'path' string -> checks file existence -> hashes content
  # Content changed -> Cache Miss -> Re-run
  expect_equal(proc(), "version2")
})

# --------------------------------------------------------#
test_that("Environment Tracking: Does NOT recurse into Locked/Package environments", {
  # We want to ensure it doesn't try to hash all of "package:base" or ".GlobalEnv"
  # which would be slow or cause infinite recursion.
  cache_dir <- setup_cache("env_safety")
  on.exit(unlink(cache_dir, recursive = TRUE))
  
  # Make function depend on a package environment
  proc <- cacheFile(cache_dir, backend="rds") %@% function() {
    # utils is a package environment
    ls(envir = as.environment("package:utils"))
    return(1)
  }
  
  # This should run quickly and NOT crash/hang trying to hash the whole utils package
  expect_equal(proc(), 1)
  
  # Verify it works for .GlobalEnv too (should skip)
  proc_global <- cacheFile(cache_dir, backend="rds") %@% function() {
    return(.GlobalEnv)
  }
  expect_type(proc_global(), "environment")
})