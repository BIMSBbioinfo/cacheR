
<!-- README.md is generated from README.Rmd. Please edit README.Rmd only. -->

# cacheR <img src="https://img.shields.io/badge/status-experimental-orange" align="right"/>

`cacheR` provides **recursive, file-based function caching** for R, with the unique
ability to record **parent–child relationships** between cached calls.

This is useful for:

- Building provenance graphs  
- Debugging nested function calls  
- Understanding how a function tree produces a result  
- Reusing heavy computations safely and transparently  

The package introduces:

- `cacheFile()` — a caching decorator  
- `%@%` — an operator for applying decorators  
- `cacheTree_*()` — functions for inspecting the cache tree  

---

## 🚀 Installation

```r
# install from GitHub
# requires: remotes::install_github()
remotes::install_github("BIMSBbioinfo/cacheR")
```

Basic usage 

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

## 📝 Future Features

- Automatic invalidation when function code changes

- Cache visualization (cacheTree_graph())

- File-space management (TTL, LRU cleanup)

- Hashing improvements and deduplication
