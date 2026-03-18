# cacheR 1.0.0

## Breaking changes

* Replaced `qs` backend with `qs2`. The `qs` package was removed from CRAN;
  its successor `qs2` is actively maintained with improved performance.
  Cache files now use the `.qs2` extension. **Old `.qs` files are not
  backward-compatible** and must be regenerated.

## New features

* **AST path spec detection** (`B3`): Function bodies are scanned for calls to
  `list.files()`, `readRDS()`, `read.csv()`, and other file-reading functions.
  Literal path arguments are hashed and included in the cache key, so the cache
  invalidates automatically when referenced files change.

* **`cache_tree_changed_files()`**: Standalone exported function that identifies
  graph nodes whose tracked files have changed on disk.

* **Edge style differentiation**: `plot_cache_graph()` now draws
  file-dependency edges as dashed lines and function-call edges as solid lines.

* **Sentinel early exit** (`B1`): The sentinel wait loop now breaks immediately
  if the `.computing` sentinel file disappears, instead of waiting until
  timeout.

* **`CACHER_DIR` environment variable** (`B6`): `cacheR_default_dir()` checks
  the `CACHER_DIR` env var before falling back to R options or the default
  directory.

* **Verbose miss reasons**: Cache miss messages now report exactly which hash
  component changed (arguments, closure, file contents, env vars, package
  versions, AST-detected paths, etc.).

## Documentation

* pkgdown site deployed via GitHub Actions at
 <http://bioinformatics.mdc-berlin.de/cacheR/>.
* Three vignettes: Introduction, RNA-seq workflow, Machine Learning workflow.
* Full function reference with roxygen documentation.

# cacheR 0.2.0

## New features

* **Cache dependency graph**: `plot_cache_graph()` visualises the live
  dependency DAG with stale-node highlighting via igraph.
* **`cacheTree_*` family**: `cacheTree_nodes()`, `cacheTree_reset()`,
  `cacheTree_save()`, `cacheTree_load()`, `cacheTree_sync()`,
  `cacheTree_for_file()` for inspecting and persisting the call tree.
* **`track_file()`**: Register external file dependencies with content hashes.
* **`export_targets_file()`**: Export the cache graph as a targets-compatible
  script.
* **`cache_stats()`**: Aggregate cache directory statistics with per-function
  breakdown.
* **`cache_file_state_info()` / `cache_file_state_clear()`**: Inspect and
  manage the in-memory file hash cache.
* **`.cacheR.yml` project config**: Load backend, verbose, env_vars settings
  from a YAML config file.
* **Sentinel-based concurrency**: `.computing` sentinel files prevent duplicate
  work across parallel workers.
* **Conditional caching, versioning, and `depends_on_files` /
  `depends_on_vars`** for explicit dependency declaration.
* **File locking** via the filelock package for safe concurrent writes.
* **Probabilistic file hashing** for fast content-change detection on large
  files.

# cacheR 0.1.0

* Initial release with `cacheFile()` decorator and `%@%` operator.
* RDS and qs backends.
* Recursive closure hashing with cycle detection.
* Automatic file path detection in arguments.
