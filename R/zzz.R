.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("cacheR.backend"))) {
    if (requireNamespace("qs", quietly = TRUE)) {
      options(cacheR.backend = "qs")
    } else {
      packageStartupMessage(
        "Package 'qs' not installed; using RDS backend.\n",
        "Install qs for faster caching: install.packages('qs')"
      )
      options(cacheR.backend = "rds")
    }
  }
  if (is.null(getOption("cacheR.dir"))) {
    options(cacheR.dir = file.path(getwd(), ".cacheR"))
  }
}
