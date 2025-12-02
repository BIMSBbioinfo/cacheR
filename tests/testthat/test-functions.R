test_that(".find_package_deps has correct signature", {
  fml <- formals(cacheR:::`.find_package_deps`)
  expect_identical(names(fml), "fun")
})