---
title: "Introduction to cacheR"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 2
vignette: >
  %\VignetteIndexEntry{Introduction to cacheR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Overview

`cacheR` provides **function-level caching** for R workflows.

Typical use cases in basic statistics:

- Expensive **summary statistics** on large datasets (mean, variance, quantiles).
- Repeated computation of **correlation matrices**.
- Multi-step pipelines, e.g. *preprocess → summarize → correlate*.

The main idea:

1. Wrap an existing function with `cacheFile()`.
2. Call the new function as usual.
3. `cacheR` saves results to disk and reuses them when inputs and function body have not changed.

---

# 1. Setup





































