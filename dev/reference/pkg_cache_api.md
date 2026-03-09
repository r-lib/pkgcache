# Functions to query and manipulate the package cache

`pkg_cache_summary()` returns a short summary of the state of the cache,
e.g. the number of files and their total size. It returns a named list.

## Usage

``` r
pkg_cache_summary(cachepath = NULL)

pkg_cache_list(cachepath = NULL)

pkg_cache_find(cachepath = NULL, ...)

pkg_cache_get_file(cachepath = NULL, target, ...)

pkg_cache_delete_files(cachepath = NULL, ...)

pkg_cache_add_file(cachepath = NULL, file, relpath = dirname(file), ...)
```

## Arguments

- cachepath:

  Path of the cache. By default the cache directory is in `pkgcache`,
  within the user's cache directory. See
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html).

- ...:

  Extra named arguments to select the package file.

- target:

  Path where the selected file is copied.

- file:

  File to add.

- relpath:

  The relative path of the file within the cache.

## See also

The
[package_cache](https://r-lib.github.io/pkgcache/dev/reference/package_cache.md)
R6 class for a more flexible API.

## Examples

``` r
if (FALSE) {
pkg_cache_summary()
pkg_cache_list()
pkg_cache_find(package = "forecast")
tmp <- tempfile()
pkg_cache_get_file(target = tmp, package = "forecast", version = "8.10")
pkg_cache_delete_files(package = "forecast")
}
```
