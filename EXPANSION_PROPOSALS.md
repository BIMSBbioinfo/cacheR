# cacheR Expansion Proposals

## Current State

The package is functionally solid (130+ tests, core caching works well) but has architectural debt.

### Critical housekeeping issues
- **4 duplicate function definitions** across `cacheFile.R`, `cacheTree.R`, and `utils.R` (`cachePrune`, `cacheInfo`, `cacheList`, `cacheTree_nodes`, `cacheR_default_dir`)
- **Dead code** -- `cache-io.R` (57 lines, never called), `graphs.R` (62 lines, entirely commented out)
- **Conflicting default dir** -- `cacheR_default_dir()` defined twice with different logic (tempdir vs cwd)
- **Inconsistent function signatures** -- `cacheInfo(path)` vs `cacheInfo(file_path)` in the two copies

---

## Tier 1: Cleanup & Consolidation (quick wins)

1. **Consolidate duplicates** -- merge `cacheTree.R` + `utils.R` into single authoritative source, delete dead files (`cache-io.R`, `graphs.R`)
2. **Fix default dir conflict** -- pick one behavior and document it
3. **LRU/bounded `.file_state_cache`** -- auto-evict after N entries instead of unbounded growth
4. **Un-export internal functions** -- `.fast_file_hash`, `.probabilistic_file_hash`, `.delayed_decorate` shouldn't be public API

## Tier 2: User Experience

5. **`cache_stats(cache_dir)`** -- total size, entry count, age distribution, estimated time saved
6. **Cache miss explanation** -- a `cache_why(cache_dir, func, ...)` that reports which input changed
7. **`.cacheR.yml` project config** -- default backend, ignore_args, env_vars per-project
8. **Better error messages** -- warn if cache_dir isn't writable, flag silent save failures

## Tier 3: New Capabilities

9. **Conditional caching** -- `force = TRUE` to bypass cache, `skip_cache = TRUE` to never write
10. **Cache versioning/tagging** -- label cache entries with version strings for explicit invalidation
11. **Dependency declaration** -- `depends_on(file, var, ...)` for non-detectable deps
12. **Incremental caching** -- cache intermediate pipeline results, not just final output
13. **Compression** -- optional gzip/zstd compression for large cache entries

## Tier 4: Ecosystem Integration

14. **`future`/parallel** -- distributed caching across workers
15. **Shiny** -- `cacheFileReactive()` for reactive contexts
16. **targets** -- bidirectional sync (not just export)
17. **Remote backends** -- S3, GCS for team-shared caches
