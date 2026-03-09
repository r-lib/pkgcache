# The R6 object that implements the global metadata cache

This is used by the
[`meta_cache_deps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md),
[`meta_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md),
etc. functions.

## Usage

``` r
get_cranlike_metadata_cache()
```

## Examples

``` r
if (FALSE) {
get_cranlike_metadata_cache()
get_cranlike_metadata_cache()$list("cli")
}
```
