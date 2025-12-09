# Create a disk-backed caching decorator for a function

`cacheFile()` creates a decorator that caches function results to files
in `cache_dir`, keyed by the function body and arguments. It also
records parent-child relationships between cached calls so that a "cache
tree" can be reconstructed.

## Usage

``` r
cacheFile(
  cache_dir = NULL,
  backend = getOption("cacheR.backend", "rds"),
  file_args = NULL
)
```

## Arguments

- cache_dir:

  Directory where cache files will be stored.

- ...:

  Additional options controlling cache behavior (e.g. hashing,
  compression, invalidation rules).

## Value

A decorator object that can be combined with a function using `%@%`,
returning a cached version of that function.

## Details

Typical usage (with the `%@%` decorator operator) is:

    inner_fun <- cacheFile(cache_dir) %@% function(x) x + 1
    outer_fun <- cacheFile(cache_dir) %@% function(x) inner_fun(x) * 2
