# Cache for CRAN archive data

This is an R6 class that implements a cache from older CRAN package
versions. For a higher level interface see the functions documented with
[`cran_archive_list()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md).

## Details

The cache is similar to
[cranlike_metadata_cache](https://r-lib.github.io/pkgcache/dev/reference/cranlike_metadata_cache.md)
and has the following layers:

- The data inside the `cran_archive_cache` object.

- Cached data in the current R session.

- An RDS file in the current session's temporary directory.

- An RDS file in the user's cache directory.

It has a synchronous and an asynchronous API.

## Usage

    cac <- cran_archive_cache$new(
      primary_path = NULL,
      replica_path = tempfile(),
      cran_mirror = default_cran_mirror(),
      update_after = as.difftime(7, units = "days"),
    )

    cac$list(packages = NULL, update_after = NULL)
    cac$async_list(packages = NULL, update_after = NULL)

    cac$update()
    cac$async_update()

    cac$check_update()
    cac$async_check_update()

    cac$summary()

    cac$cleanup(force = FALSE)

## Arguments

- `primary_path`: Path of the primary, user level cache. Defaults to the
  user level cache directory of the machine.

- `replica_path`: Path of the replica. Defaults to a temporary directory
  within the session temporary directory.

- `cran_mirror`: CRAN mirror to use, this takes precedence over `repos`.

- `update_after`: `difftime` object. Automatically update the cache if
  it gets older than this. Set it to `Inf` to avoid updates. Defaults to
  seven days.

- `packages`: Packages to query, character vector.

- `force`: Whether to force cleanup without asking the user.

## Details

Create a new archive cache with `cran_archive_cache$new()`. Multiple
caches are independent, so e.g. if you update one of them, the other
existing caches are not affected.

`cac$list()` lists the versions of the specified packages, or all
packages, if none were specified. `cac$async_list()` is the same, but
asynchronous.

`cac$update()` updates the cache. It always downloads the new metadata.
`cac$async_update()` is the same, but asynchronous.

`cac$check_update()` updates the cache if there is a newer version
available. `cac$async_check_update()` is the same, but asynchronous.

`cac$summary()` returns a summary of the archive cache, a list with
entries:

- `cachepath`: path to the directory of the main archive cache,

- `current_rds`: the RDS file that stores the cache. (This file might
  not exist, if the cache is not downloaded yet.)

- `lockfile`: the file used for locking the cache.

- \`timestamp: time stamp for the last update of the cache.

- `size`: size of the cache file in bytes.

`cac$cleanup()` cleans up the cache files.

## Columns

`cac$list()` returns a data frame with columns:

- `package`: package name,

- `version`: package version. This is a character vector, and not a
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html)
  object. Some older package versions are not supported by
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html).

- `raw`: the raw row names from the CRAN metadata.

- `mtime`: `mtime` column from the CRAN metadata. This is usually pretty
  close to the release date and time of the package.

- `url`: package download URL.

- `mirror`: CRAN mirror that was used to get this data.

## Examples

``` r
arch <- cran_archive_cache$new()
arch$update()
#> # A data frame: 170,926 × 6
#>    package  version raw                mtime               url   mirror
#>  * <chr>    <chr>   <chr>              <dttm>              <chr> <chr> 
#>  1 A3       0.9.1   A3/A3_0.9.1.tar.gz 2013-02-07 09:00:29 http… https…
#>  2 A3       0.9.2   A3/A3_0.9.2.tar.gz 2013-03-26 18:58:40 http… https…
#>  3 A3       1.0.0   A3/A3_1.0.0.tar.gz 2015-08-16 21:05:54 http… https…
#>  4 aamatch  0.3.7   aamatch/aamatch_0… 2025-06-24 09:40:05 http… https…
#>  5 aaMI     1.0-0   aaMI/aaMI_1.0-0.t… 2005-06-24 15:55:17 http… https…
#>  6 aaMI     1.0-1   aaMI/aaMI_1.0-1.t… 2005-10-17 19:24:18 http… https…
#>  7 aaSEA    1.0.0   aaSEA/aaSEA_1.0.0… 2019-08-01 09:10:08 http… https…
#>  8 aaSEA    1.1.0   aaSEA/aaSEA_1.1.0… 2019-11-09 16:20:04 http… https…
#>  9 AATtools 0.0.1   AATtools/AATtools… 2020-06-14 15:10:07 http… https…
#> 10 AATtools 0.0.2   AATtools/AATtools… 2022-08-12 13:40:11 http… https…
#> # ℹ 170,916 more rows
arch$list()
#> # A data frame: 170,926 × 6
#>    package  version raw                mtime               url   mirror
#>  * <chr>    <chr>   <chr>              <dttm>              <chr> <chr> 
#>  1 A3       0.9.1   A3/A3_0.9.1.tar.gz 2013-02-07 09:00:29 http… https…
#>  2 A3       0.9.2   A3/A3_0.9.2.tar.gz 2013-03-26 18:58:40 http… https…
#>  3 A3       1.0.0   A3/A3_1.0.0.tar.gz 2015-08-16 21:05:54 http… https…
#>  4 aamatch  0.3.7   aamatch/aamatch_0… 2025-06-24 09:40:05 http… https…
#>  5 aaMI     1.0-0   aaMI/aaMI_1.0-0.t… 2005-06-24 15:55:17 http… https…
#>  6 aaMI     1.0-1   aaMI/aaMI_1.0-1.t… 2005-10-17 19:24:18 http… https…
#>  7 aaSEA    1.0.0   aaSEA/aaSEA_1.0.0… 2019-08-01 09:10:08 http… https…
#>  8 aaSEA    1.1.0   aaSEA/aaSEA_1.1.0… 2019-11-09 16:20:04 http… https…
#>  9 AATtools 0.0.1   AATtools/AATtools… 2020-06-14 15:10:07 http… https…
#> 10 AATtools 0.0.2   AATtools/AATtools… 2022-08-12 13:40:11 http… https…
#> # ℹ 170,916 more rows
```
