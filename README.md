
<!-- README.md is generated from README.Rmd. Please edit README.Rmd only. -->

# cacheR <img src="https://img.shields.io/badge/status-experimental-orange" align="right"/>

**cacheR** tracks your data and code so you don't have to

## What does cacheR do?

It automatically checks for changes in code and input data and re-runs the code if necessary.

It's like snakemake/nextflow, but on the fly

<p align="center">
  <img src="../Results/cacheR_graph_animation.gif" alt="cacheR cache graph animation" width="700"/>
</p>

## What is it useful for?

- Keeping the analysis up to date

- Saving time

- Not using obsolete results 

- Reusing heavy computations safely and transparently  

 

---

### 🚀 Installation

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

### How does cacheR decide to recompute?

A cached call is reused **only if** all of the following are unchanged:

- The **function body** (including inline code changes)
- The **arguments** (up to hashing / comparison rules)
- The **tracked files / directories**, where relevant
- The **package versions** of any non-base functions used
- The **environment variables** used by the function

If any of these change, cacheR invalidates the old entry and recomputes.


### Limitations & caveats

- **Package boundaries:**  
  cacheR stops tracking when it hits a function imported from a package.  
  Instead, it records the package name and version. It does not inspect the
  internals of those functions.

- **Native code / C / external tools:**  
  C/C++ code and external tools (e.g. `system("bwa mem ...")`) are not tracked.
  If they change, cacheR will not notice unless their inputs / outputs change
  in a tracked place.

- **Side effects:**  
  Functions with side effects (writing to global variables, random seeds,
  databases, etc.) are not fully “safe” to cache. Prefer pure, data-in/data-out
  functions.

#### When you probably *shouldn’t* use cacheR

- Highly stateful / interactive code where caching would confuse you more
  than it helps
- Situations where you need full workflow orchestration, scheduling, and
  cluster execution (use snakemake/nextflow/targets/etc. instead)

