## ---- setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(cacheR)

## -----------------------------------------------------------------------------
cache_dir <- file.path(tempdir(), "cacheR-demo")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
cache_dir

## -----------------------------------------------------------------------------

stat_summary <- function(x) {
  message("Computing stats ...")
  Sys.sleep(1)  # pretend it's expensive
  x <- na.omit(x)
  list(
    mean     = mean(x),
    variance = var(x),
    sd       = sd(x),
    median   = median(x)
  )
}

## -----------------------------------------------------------------------------

cached_stat_summary <- cacheFile(
  cache_dir = cache_dir,
  backend   = "rds"
) %@% function(x) {
  stat_summary(x)
}

## -----------------------------------------------------------------------------

set.seed(1)
x <- rnorm(1e5)

system.time(res1 <- cached_stat_summary(x))
system.time(res2 <- cached_stat_summary(x))

res1
identical(res1, res2)

## -----------------------------------------------------------------------------

cor_with_moments <- function(mat) {
  message("Computing correlations and moments ...")
  Sys.sleep(1)  # simulate expensive work

  # Basic checks
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  mat <- as.matrix(mat)

  means     <- colMeans(mat, na.rm = TRUE)
  variances <- apply(mat, 2, var, na.rm = TRUE)

  corr_mat  <- stats::cor(mat, use = "pairwise.complete.obs")

  list(
    mean      = means,
    variance  = variances,
    cor       = corr_mat
  )
}

## -----------------------------------------------------------------------------

cached_cor_with_moments <- cacheFile(
  cache_dir = cache_dir,
  backend   = "rds"
) %@% function(mat) {
  cor_with_moments(mat)
}

## -----------------------------------------------------------------------------

set.seed(123)
mat <- matrix(rnorm(200 * 10), ncol = 10)

system.time(res1 <- cached_cor_with_moments(mat))
system.time(res2 <- cached_cor_with_moments(mat))

str(res1$cor)
identical(res1$cor, res2$cor)

## -----------------------------------------------------------------------------
preprocess_matrix <- function(mat) {
  message("Preprocessing matrix (center & scale) ...")
  Sys.sleep(1)  # simulate expensive step
  scale(mat)
}

# Cached preprocessing
cached_preprocess_matrix <- cacheFile(
  cache_dir = cache_dir
) %@% function(mat) {
  preprocess_matrix(mat)
}

## -----------------------------------------------------------------------------
analyze_scaled <- cacheFile(
  cache_dir = cache_dir
) %@% function(mat) {
  message("Analyzing scaled data ...")
  mat_scaled <- cached_preprocess_matrix(mat)

  means     <- colMeans(mat_scaled)
  variances <- apply(mat_scaled, 2, var)
  corr_mat  <- stats::cor(mat_scaled)

  list(
    mean     = means,
    variance = variances,
    cor      = corr_mat
  )
}

## -----------------------------------------------------------------------------
set.seed(321)
mat2 <- matrix(rnorm(300 * 8), ncol = 8)

system.time(a1 <- analyze_scaled(mat2))
system.time(a2 <- analyze_scaled(mat2))

identical(a1$cor, a2$cor)

## -----------------------------------------------------------------------------
cacheTree_reset()
a <- analyze_scaled(mat2)
nodes <- cacheTree_nodes()
nodes

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("qs")

## -----------------------------------------------------------------------------
cached_cor_qs <- cacheFile(
  cache_dir = cache_dir,
  backend   = "qs"
) %@% function(mat) {
  cor_with_moments(mat)
}

## -----------------------------------------------------------------------------
set.seed(999)
mat_big <- matrix(rnorm(500 * 50), ncol = 50)

system.time(r1 <- cached_cor_qs(mat_big))
system.time(r2 <- cached_cor_qs(mat_big))

identical(r1$cor, r2$cor)
list.files(cache_dir, recursive = TRUE)

## ----make-example-csv, message=FALSE------------------------------------------
# We use tempdir() so that the vignette can be built anywhere
# without needing any external data files.

set.seed(1)

# Small numeric toy dataset
my_data <- data.frame(
  x = rnorm(100, mean = 0, sd = 1),
  y = rnorm(100, mean = 5, sd = 2),
  z = rnorm(100, mean = -2, sd = 0.5)
)

# Path where we'll save the CSV
example_csv <- file.path(tempdir(), "my_data.csv")

# Write the CSV (no row names to keep it clean)
write.csv(my_data, example_csv, row.names = FALSE)

## -----------------------------------------------------------------------------
read_numeric_csv <- function(path) {
  message("Reading CSV: ", path)
  Sys.sleep(1)  # simulate slow disk / network
  read.csv(path)
}

cached_read_numeric_csv <- cacheFile(
  cache_dir = cache_dir
) %@% function(path) {
  read_numeric_csv(path)
}

cached_file_stats <- cacheFile(
  cache_dir = cache_dir
) %@% function(path) {
  df <- cached_read_numeric_csv(path)
  cor_with_moments(df)
}

## -----------------------------------------------------------------------------
res <- cached_file_stats(example_csv)
res$mean
res$cor

