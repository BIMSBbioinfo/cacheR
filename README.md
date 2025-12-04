
<!-- README.md is generated from README.Rmd. Please edit README.Rmd only. -->

# cacheR <img src="https://img.shields.io/badge/status-experimental-orange" align="right"/>

**cacheR** tracks your data and code so you don't have to

## What does cacheR do?

It automatically checks for changes in code and input data and re-runs the code if necessary.

It's like snakemake/nextflow, but on the fly

## What is it useful for?

- Keeping the analysis up to date
- Saving time
- Not using obsolete results 
- Reusing heavy computations safely and transparently  

 

---

## 🚀 Installation

```r
# install from GitHub
# requires: remotes::install_github()
remotes::install_github("BIMSBbioinfo/cacheR")
```

### Basic usage 

The package introduces:

- `cacheFile()` — a caching decorator  
- `%@%` — an operator for applying decorators  
- `cacheTree_*()` — functions for inspecting the cache tree 

```
library(cacheR)

cache_dir <- file.path(tempdir(), "cache_test")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Define cached functions
inner <- cacheFile(cache_dir) %@% function(x) x + 1
outer <- cacheFile(cache_dir) %@% function(x) inner(x) * 2

# Execute
outer(3)
#> 8

```
