# Decorator application operator

Applies a decorator returned by
[`cacheFile()`](https://BIMSBbioinfo.github.io/cacheR/reference/cacheFile.md)
to a function.

## Usage

``` r
decorator %@% f
```

## Arguments

- decorator:

  A decorator object, such as that returned by
  [`cacheFile()`](https://BIMSBbioinfo.github.io/cacheR/reference/cacheFile.md).

- fun:

  A function to decorate.

## Value

A new function with caching behavior.
