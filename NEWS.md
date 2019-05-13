
# pkgcache 1.0.4

* Fix handling of Bioconducor versions and repositories, see
  README for the details.

* Now different versions of pkgcache, that potentially have different
  metadata format, can share the same metadata cache directory.

# pkgcache 1.0.3

* Fix concurrency issues when the async API is used multiple times in the
  same event loop.

* Make package compatible with tibble >= 2.0.0.

* Add `meta_cache_summary()` and a `summary()` method for
  `cranlike_metadata_cache`. Return information about a metadata cache
  instance.

# pkgcache 1.0.2

* First public release
