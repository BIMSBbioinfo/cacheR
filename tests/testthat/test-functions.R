test_that(".find_package_deps has correct signature", {
  fml <- formals(cacheR:::`.find_package_deps`)
  expect_identical(names(fml), "fun")
})

test_that(".find_package_deps handles internal functions with missing arguments (Regression)", {
  # This function body contains a function definition with no default value for 'x'
  # previously, this caused a crash in .find_package_deps
  f <- function() {
    inner <- function(x) {
      length(x)
    }
    inner(1)
  }

  expect_error(cacheR:::`.find_package_deps`(f), NA)
  
  # Ensure it also works with alist (often used in metaprogramming)
  f_alist <- function() {
    res <- alist(a =, b = 1)
    return(res)
  }
  expect_error(cacheR:::`.find_package_deps`(f_alist), NA)
})