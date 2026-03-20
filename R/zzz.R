#' Load .cacheR.yml config from working directory
#'
#' Reads a YAML config file and sets options that haven't already been set
#' by the user. Precedence: explicit function args > R options > .cacheR.yml > defaults.
#'
#' @param path Path to the YAML config file.
#' @return Invisible NULL.
#' @keywords internal
.load_cacheR_config <- function(path = file.path(getwd(), ".cacheR.yml")) {
  if (!file.exists(path)) return(invisible(NULL))
  if (!requireNamespace("yaml", quietly = TRUE)) return(invisible(NULL))

  config <- tryCatch(yaml::read_yaml(path), error = function(e) {
    warning("cacheR: failed to read .cacheR.yml: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (!is.list(config)) return(invisible(NULL))

  # Only set options not already configured by the user
  config_map <- list(
    cache_dir = "cacheR.dir",
    backend   = "cacheR.backend",
    verbose   = "cacheR.verbose",
    env_vars  = "cacheR.env_vars"
  )

  for (key in names(config_map)) {
    opt_name <- config_map[[key]]
    if (!is.null(config[[key]]) && is.null(getOption(opt_name))) {
      opts <- list(config[[key]])
      names(opts) <- opt_name
      do.call(options, opts)
    }
  }

  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  # 1. Load .cacheR.yml config (only sets options not already set by user)
  .load_cacheR_config()

  # 2. Set defaults for anything still unset
  if (is.null(getOption("cacheR.backend"))) {
    if (requireNamespace("qs2", quietly = TRUE)) {
      options(cacheR.backend = "qs2")
    } else {
      options(cacheR.backend = "rds")
    }
  }
  if (is.null(getOption("cacheR.dir"))) {
    options(cacheR.dir = file.path(getwd(), ".cacheR"))
  }
}

.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("qs2", quietly = TRUE)) {
    packageStartupMessage(
      "Package 'qs2' not installed; using RDS backend.\n",
      "Install qs2 for faster caching: install.packages('qs2')"
    )
  }
}
