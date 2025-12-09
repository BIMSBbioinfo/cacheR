# Introduction to cacheR

## Overview

`cacheR` provides **function-level caching** for R workflows.

Typical use cases in basic statistics:

- Expensive **summary statistics** on large datasets (mean, variance,
  quantiles).
- Repeated computation of **correlation matrices**.
- Multi-step pipelines, e.g. *preprocess → summarize → correlate*.

The main idea:

1.  Wrap an existing function with
    [`cacheFile()`](https://BIMSBbioinfo.github.io/cacheR/reference/cacheFile.md).
2.  Call the new function as usual.
3.  `cacheR` saves results to disk and reuses them when inputs and
    function body have not changed.

------------------------------------------------------------------------

## 1. Setup

``` r

library(cacheR)
```

For this vignette we’ll use a **temporary cache directory**:

``` r

cache_dir <- file.path(tempdir(), "cacheR-demo")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
cache_dir
#> [1] "/local/vfranke/Tmp/RtmpVJ2w7C/cacheR-demo"
```

------------------------------------------------------------------------

## 2. Caching basic summary statistics

Assume we have a function that computes several statistics at once:

``` r


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
```

We create a **cached** version:

``` r


cached_stat_summary <- cacheFile(
  cache_dir = cache_dir,
  backend   = "rds"
) %@% function(x) {
  stat_summary(x)
}
```

Now, compare first vs second call:

``` r


set.seed(1)
x <- rnorm(1e5)

system.time(res1 <- cached_stat_summary(x))
#> Computing stats ...
#>    user  system elapsed 
#>   0.009   0.000   1.010
system.time(res2 <- cached_stat_summary(x))
#>    user  system elapsed 
#>   0.001   0.000   0.001

res1
#> $mean
#> [1] -0.002244083
#> 
#> $variance
#> [1] 1.007059
#> 
#> $sd
#> [1] 1.003523
#> 
#> $median
#> [1] 0.0008011329
identical(res1, res2)
#> [1] TRUE
```

- First call: prints `"Computing stats ..."` and takes ~1 second.
- Second call with the *same* input: should be almost instant, reusing
  the cached result.

------------------------------------------------------------------------

## 3. Caching a correlation matrix

Computing a full correlation matrix can be expensive for many
variables.  
Let’s define a function that:

- standardizes the columns,
- computes the correlation matrix,
- optionally returns means and variances as well.

``` r


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
```

Wrap it with
[`cacheFile()`](https://BIMSBbioinfo.github.io/cacheR/reference/cacheFile.md):

``` r


cached_cor_with_moments <- cacheFile(
  cache_dir = cache_dir,
  backend   = "rds"
) %@% function(mat) {
  cor_with_moments(mat)
}
```

Test it:

``` r


set.seed(123)
mat <- matrix(rnorm(200 * 10), ncol = 10)

system.time(res1 <- cached_cor_with_moments(mat))
#> Computing correlations and moments ...
#>    user  system elapsed 
#>   0.004   0.000   1.004
system.time(res2 <- cached_cor_with_moments(mat))
#>    user  system elapsed 
#>   0.000   0.000   0.001

str(res1$cor)
#>  num [1:10, 1:10] 1 -0.0277 -0.0302 -0.0464 -0.0513 ...
identical(res1$cor, res2$cor)
#> [1] TRUE
```

Again:

- First call: actually computes mean, variance and correlation.
- Second call: returns the result from disk, skipping the expensive
  computation.

------------------------------------------------------------------------

## 4. Nested caching: reuse pre-processing

Suppose you often apply the same pre-processing (centering & scaling)
before computing summary statistics or correlations.

Define a **preprocessing** function and a **downstream** function that
uses it:

``` r

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
```

Now a downstream function that uses the preprocessed data to compute
means, variances and correlations:

``` r

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
```

Run it twice:

``` r

set.seed(321)
mat2 <- matrix(rnorm(300 * 8), ncol = 8)

system.time(a1 <- analyze_scaled(mat2))
#> Analyzing scaled data ...
#> Preprocessing matrix (center & scale) ...
#>    user  system elapsed 
#>   0.006   0.000   1.007
system.time(a2 <- analyze_scaled(mat2))
#>    user  system elapsed 
#>   0.001   0.000   0.001

identical(a1$cor, a2$cor)
#> [1] TRUE
```

What happens:

1.  **First run**
    - `cached_preprocess_matrix()` is executed and cached.
    - `analyze_scaled()` is executed and cached.
2.  **Second run**
    - Both the preprocessing step and the full analysis should be loaded
      from cache (assuming the function bodies and input matrix are
      unchanged).

If your version of `cacheR` supports a **cache tree**, you can inspect
the relationships between cached calls:

``` r

cacheTree_reset()
a <- analyze_scaled(mat2)
nodes <- cacheTree_nodes()
nodes
#> $`analyze_scaled:88c7ed8bc6ae408d`
#> $`analyze_scaled:88c7ed8bc6ae408d`$id
#> [1] "analyze_scaled:88c7ed8bc6ae408d"
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$fname
#> [1] "analyze_scaled"
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$hash
#> [1] "88c7ed8bc6ae408d"
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$outfile
#> [1] "/local/vfranke/Tmp/RtmpVJ2w7C/cacheR-demo/analyze_scaled.88c7ed8bc6ae408d.qs"
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$parents
#> character(0)
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$children
#> character(0)
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$files
#> character(0)
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$file_hashes
#> character(0)
#> 
#> $`analyze_scaled:88c7ed8bc6ae408d`$created
#> [1] "2025-12-02 16:31:40 CET"
```

You should see at least two nodes:

- one for `analyze_scaled(mat2)`
- one for `cached_preprocess_matrix(mat2)`

------------------------------------------------------------------------

## 5. Using the `qs` backend for faster serialization

For large matrices, the [`qs`](https://github.com/qsbase/qs) backend can
be faster than base RDS.

Install it once:

``` r

install.packages("qs")
```

Then create a cached function with `backend = "qs"`:

``` r

cached_cor_qs <- cacheFile(
  cache_dir = cache_dir,
  backend   = "qs"
) %@% function(mat) {
  cor_with_moments(mat)
}
```

Compare timings:

``` r

set.seed(999)
mat_big <- matrix(rnorm(500 * 50), ncol = 50)

system.time(r1 <- cached_cor_qs(mat_big))
#> Computing correlations and moments ...
#>    user  system elapsed 
#>   0.017   0.000   1.019
system.time(r2 <- cached_cor_qs(mat_big))
#>    user  system elapsed 
#>   0.001   0.000   0.001

identical(r1$cor, r2$cor)
#> [1] TRUE
list.files(cache_dir, recursive = TRUE)
#> [1] "analyze_scaled.88c7ed8bc6ae408d.qs"          
#> [2] "cached_cor_qs.9a5887591cc30d22.qs"           
#> [3] "cached_cor_with_moments.458b9d5fd0c6e8da.rds"
#> [4] "cached_preprocess_matrix.eb193d19e30e3ec8.qs"
#> [5] "cached_stat_summary.d199ddc8223d7981.rds"
```

You should see some `.qs` files in the cache directory.

------------------------------------------------------------------------

## 6. Combining file I/O and statistics

Create an example CSV file

``` r

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
```

In practice, you often:

1.  Read data from a file (CSV, RDS, etc.),
2.  Compute summary statistics or correlations.

You can cache both steps.

``` r

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
```

Usage:

``` r

res <- cached_file_stats(example_csv)
#> Reading CSV: /local/vfranke/Tmp/RtmpVJ2w7C/my_data.csv
#> Computing correlations and moments ...
res$mean
#>          x          y          z 
#>  0.1088874  4.9243838 -1.9851632
res$cor
#>               x             y           z
#> x  1.0000000000 -0.0009943199  0.01838219
#> y -0.0009943199  1.0000000000 -0.04953621
#> z  0.0183821868 -0.0495362135  1.00000000
```

- First run: reads the CSV and computes stats.
- Subsequent runs with the same file and same code: reuse cached
  results.

(Depending on your `cacheR` version, there may be options such as
`file_args` to incorporate directory or file contents into the cache key
more explicitly.)

------------------------------------------------------------------------

## 7. Practical tips

- Use a project-local cache directory, e.g.:

  ``` r

  cache_dir <- ".cacheR"
  dir.create(cache_dir, showWarnings = FALSE)
  ```

- Add it to `.gitignore`:

  ``` text
  .cacheR/
  ```

- Wrap **expensive** operations:

  - reading large files
  - computing big correlation matrices
  - heavy preprocessing

- For pure statistical pipelines, a common pattern is:

  1.  `cached_read()` – load data
  2.  `cached_preprocess()` – clean / transform
  3.  `cached_stats()` – compute means, variances, correlations

  Each step can be shared across many analyses.

------------------------------------------------------------------------
