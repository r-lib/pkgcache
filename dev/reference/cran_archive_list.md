# Data about older versions of CRAN packages

CRAN mirrors store older versions of packages in `/src/contrib/Archive`,
and they also store some metadata about them in
`/src/contrib/Meta/archive.rds`. pkgcache can download and cache this
metadata.

## Usage

``` r
cran_archive_list(
  cran_mirror = default_cran_mirror(),
  update_after = as.difftime(7, units = "days"),
  packages = NULL
)

cran_archive_update(cran_mirror = default_cran_mirror())

cran_archive_cleanup(cran_mirror = default_cran_mirror(), force = FALSE)

cran_archive_summary(cran_mirror = default_cran_mirror())
```

## Arguments

- cran_mirror:

  CRAN mirror to use, see
  [`default_cran_mirror()`](https://r-lib.github.io/pkgcache/dev/reference/default_cran_mirror.md).

- update_after:

  `difftime` object. Automatically update the cache if it gets older
  than this. Set it to `Inf` to avoid updates. Defaults to seven days.

- packages:

  Character vector. Only report these packages.

- force:

  Force cleanup in non-interactive mode.

## Value

`cran_archive_list()` returns a data frame with columns:

- `package`: package name,

- `version`: package version. This is a character vector, and not a
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html)
  object. Some older package versions are not supported by
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html).

- `raw`: the raw row names from the CRAN metadata.

- `mtime`: `mtime` column from the CRAN metadata. This is usually pretty
  close to the release date and time of the package.

- `url`: package download URL.

- `mirror`: CRAN mirror that was used to get this data. The output is
  ordered according to package names (case insensitive) and release
  dates.

`cran_archive_update()` returns all archive data in a data frame, in the
same format as `cran_archive_list()`, invisibly.

`cran_archive_cleanup()` returns nothing.

`cran_archive_summary()` returns a named list with elements:

- `cachepath`: Path to the directory that contains all archive cache.

- `current_rds`: Path to the RDS file that contains the data for the
  specified `cran_mirror`.

- `lockfile`: Path to the lock file for `current_rds`.

- `timestamp`: Path to the time stamp for `current_rds`. `NA` if the
  cache is empty.

- `size`: Size of `current_rds`. Zero if the cache is empty.

## Details

`cran_archive_list()` lists all versions of all (or some) packages. It
updates the cached data first, if it is older than the specified limit.

`cran_archive_update()` updates the archive cache.

`cran_archive_cleanup()` cleans up the archive cache for `cran_mirror`.

`cran_archive_summary()` prints a summary about the archive cache.

## See also

The `cran_archive_cache` class for more flexibility.

## Examples

``` r
cran_archive_list(packages = "readr")
#> # A data frame: 23 × 6
#>    package version raw                 mtime               url   mirror
#>  * <chr>   <chr>   <chr>               <dttm>              <chr> <chr> 
#>  1 readr   0.1.0   readr/readr_0.1.0.… 2015-04-08 23:03:56 http… https…
#>  2 readr   0.1.1   readr/readr_0.1.1.… 2015-05-29 14:27:00 http… https…
#>  3 readr   0.2.0   readr/readr_0.2.0.… 2015-10-20 09:44:22 http… https…
#>  4 readr   0.2.1   readr/readr_0.2.1.… 2015-10-21 08:02:08 http… https…
#>  5 readr   0.2.2   readr/readr_0.2.2.… 2015-10-22 06:24:39 http… https…
#>  6 readr   1.0.0   readr/readr_1.0.0.… 2016-08-03 15:55:27 http… https…
#>  7 readr   1.1.0   readr/readr_1.1.0.… 2017-03-22 19:24:25 http… https…
#>  8 readr   1.1.1   readr/readr_1.1.1.… 2017-05-16 19:04:00 http… https…
#>  9 readr   1.2.0   readr/readr_1.2.0.… 2018-11-22 08:40:08 http… https…
#> 10 readr   1.2.1   readr/readr_1.2.1.… 2018-11-22 14:30:05 http… https…
#> # ℹ 13 more rows
```
