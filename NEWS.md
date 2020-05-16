
# pkgcache 1.0.7

* Metadata is now cached in RDS version 2 formats, so metadata written
  by newer R version can be used by older R versions as well (#36).

# pkgcache 1.0.6

* HTTP timeouts are now much better, and by default they are defined
  in terms of download speed, instead of total download time (#29).

* pkgcache now tries to download metadata from the `PACKAGES` file, if it
  cannot find `PACKAGES.gz` (@timmsm, #27).

* pkgcache is now less verbose when updating or loading metadata.

# pkgcache 1.0.5

* Fix a bug in the download functions, that broke pak downloads.

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
