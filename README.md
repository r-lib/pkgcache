
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgcache

> Cache CRAN-like metadata and package files

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgcache)](https://cran.r-project.org/package=pkgcache)
[![R build
status](https://github.com/r-lib/pkgcache/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/pkgcache/actions)
[![Coverage
status](https://codecov.io/gh/r-lib/pkgcache/branch/main/graph/badge.svg)](https://codecov.io/github/r-lib/pkgcache?branch=main)
<!-- badges: end -->

Metadata and package cache for CRAN-like repositories. This is a utility
package to be used by package management tools that want to take
advantage of caching.

## Installation

You can install the released version of pkgcache from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pkgcache")
```

## Metadata cache

`meta_cache_list()` lists all packages in the metadata cache. It
includes Bioconductor package, and all versions (i.e. both binary and
source) of the packages for the current platform and R version.

``` r
library(pkgcache)
meta_cache_list()
#> # A tibble: 40,825 x 32
#>    package  version depends  suggests  license imports  linkingto archs enhances
#>    <chr>    <chr>   <chr>    <chr>     <chr>   <chr>    <chr>     <chr> <chr>   
#>  1 A3       1.0.0   R (>= 2~ randomFo~ GPL (>~ <NA>     <NA>      <NA>  <NA>    
#>  2 AATtools 0.0.1   R (>= 3~ <NA>      GPL-3   magritt~ <NA>      <NA>  <NA>    
#>  3 ABACUS   1.0.0   R (>= 3~ rmarkdow~ GPL-3   ggplot2~ <NA>      <NA>  <NA>    
#>  4 ABC.RAP  0.9.0   R (>= 3~ knitr, r~ GPL-3   graphic~ <NA>      <NA>  <NA>    
#>  5 ABCanal~ 1.2.1   R (>= 2~ <NA>      GPL-3   plotrix  <NA>      <NA>  <NA>    
#>  6 ABCoptim 0.15.0  <NA>     testthat~ MIT + ~ Rcpp, g~ Rcpp      ABCo~ <NA>    
#>  7 ABCp2    1.2     MASS     <NA>      GPL-2   <NA>     <NA>      <NA>  <NA>    
#>  8 ABHgeno~ 1.0.1   <NA>     knitr, r~ GPL-3   ggplot2~ <NA>      <NA>  <NA>    
#>  9 ABPS     0.3     <NA>     testthat  GPL (>~ kernlab  <NA>      <NA>  <NA>    
#> 10 ACA      1.1     R (>= 3~ <NA>      GPL     graphic~ <NA>      <NA>  <NA>    
#> # ... with 40,815 more rows, and 23 more variables: os_type <chr>,
#> #   priority <chr>, license_is_foss <chr>, license_restricts_use <chr>,
#> #   repodir <chr>, rversion <chr>, platform <chr>, needscompilation <chr>,
#> #   ref <chr>, type <chr>, direct <lgl>, status <chr>, target <chr>,
#> #   mirror <chr>, sources <list>, filesize <dbl>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>, path <chr>
```

`meta_cache_deps()` and `meta_cache_revdeps()` can be used to look up
dependencies and reverse dependencies.

The metadata is updated automatically if it is older than seven days,
and it can also be updated manually with `meta_cache_update()`.

See the `cranlike_metadata_cache` R6 class for a lower level API, and
more control.

## Package cache

Package management tools may use the `pkg_cache_*` functions and in
particular the `package_cache` class, to make use of local caching of
package files.

The `pkg_cache_*` API is high level, and uses a user level cache:

``` r
pkg_cache_summary()
#> $cachepath
#> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/pkg"
#> 
#> $files
#> [1] 203
#> 
#> $size
#> [1] 112241143
```

``` r
pkg_cache_list()
#> # A tibble: 203 x 11
#>    fullpath  path   package  url   etag  sha256 version platform built vignettes
#>    <chr>     <glue> <chr>    <chr> <chr> <chr>  <chr>   <chr>    <int>     <int>
#>  1 /Users/g~ bin/m~ lifecyc~ http~ "\"1~ 2a93c~ 1.0.1   aarch64~    NA        NA
#>  2 /Users/g~ bin/m~ tibble   http~ "\"c~ 61fd0~ 3.1.6   aarch64~    NA        NA
#>  3 /Users/g~ src/c~ dplyr    <NA>   <NA> fc181~ <NA>    <NA>         0        NA
#>  4 /Users/g~ src/c~ rlang    <NA>   <NA> 20b7f~ <NA>    <NA>         0        NA
#>  5 /Users/g~ bin/m~ askpass  http~ "\"5~ f724f~ 1.1     macos       NA        NA
#>  6 /Users/g~ bin/m~ cachem   http~ "\"1~ 4efc0~ 1.0.6   macos       NA        NA
#>  7 /Users/g~ bin/m~ brio     http~ "\"9~ 623ea~ 1.1.2   macos       NA        NA
#>  8 /Users/g~ bin/m~ clipr    http~ "\"b~ 65f4b~ 0.7.1   macos       NA        NA
#>  9 /Users/g~ bin/m~ brew     http~ "\"1~ bf6da~ 1.0-6   macos       NA        NA
#> 10 /Users/g~ bin/m~ commonm~ http~ "\"4~ dccce~ 1.7     macos       NA        NA
#> # ... with 193 more rows, and 1 more variable: rversion <int>
```

``` r
pkg_cache_find(package = "dplyr")
#> # A tibble: 2 x 11
#>   fullpath  path   package url     etag  sha256 version platform built vignettes
#>   <chr>     <glue> <chr>   <chr>   <chr> <chr>  <chr>   <chr>    <int>     <int>
#> 1 /Users/g~ src/c~ dplyr   <NA>     <NA> fc181~ <NA>    <NA>         0        NA
#> 2 /Users/g~ bin/m~ dplyr   https:~ "\"1~ e46b3~ 1.0.7   aarch64~    NA        NA
#> # ... with 1 more variable: rversion <int>
```

`pkg_cache_add_file()` can be used to add a file,
`pkg_cache_delete_files()` to remove files, `pkg_cache_get_files()` to
copy files out of the cache.

The `package_cache` class provides a finer API.

## Bioconductor support

Both the metadata cache and the package cache support Bioconductor by
default, automatically. See the `BioC_mirror` option and the
`R_BIOC_MIRROR` and `R_BIOC_VERSION` environment variables below to
configure Bioconductor support.

## Package Options

-   The `BioC_mirror` option can be used to select a Bioconductor
    mirror. This takes priority over the `R_BIOC_MIRROR` environment
    variable.
-   `pkgcache_timeout` is the HTTP timeout for all downloads. It is in
    seconds, and the limit for downloading the whole file. Defaults to
    3600, one hour. It corresponds to the [`TIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html).
-   `pkgcache_connecttimeout` is the HTTP timeout for the connection
    phase. It is in seconds and defaults to 30 seconds. It corresponds
    to the [`CONNECTTIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html).
-   `pkgcache_low_speed_limit` and `pkgcache_low_speed_time` are used
    for a more sensible HTTP timeout. If the download speed is less than
    `pkgcache_low_speed_limit` bytes per second for at least
    `pkgcache_low_speed_time` seconds, the download errors. They
    correspond to the
    [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
    and
    [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
    curl options.

## Package environment variables

-   The `R_BIOC_VERSION` environment variable can be used to override
    the default Bioconductor version detection and force a given
    version. E.g. this can be used to force the development version of
    Bioconductor.
-   The `R_BIOC_MIRROR` environment variable can be used to select a
    Bioconductor mirror. The `BioC_mirror` option takes priority over
    this, if set.
-   `PKGCACHE_TIMEOUT` is the HTTP timeout for all downloads. It is in
    seconds, and the limit for downloading the whole file. Defaults to
    3600, one hour. It corresponds to the [`TIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html). The
    `pkgcache_timeout` option has priority over this, if set.
-   `PKGCACHE_CONNECTTIMEOUT` is the HTTP timeout for the connection
    phase. It is in seconds and defaults to 30 seconds. It corresponds
    to the [`CONNECTTIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html). The
    `pkgcache_connecttimeout` option takes precedence over this, if set.
-   `PKGCACHE_LOW_SPEED_LIMIT` and `PKGCACHE_LOW_SPEED_TIME` are used
    for a more sensible HTTP timeout. If the download speed is less than
    `PKGCACHE_LOW_SPEED_LIMIT` bytes per second for at least
    `PKGCACHE_LOW_SPEED_TIME` seconds, the download errors. They
    correspond to the
    [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
    and
    [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
    curl options. The `pkgcache_low_speed_time` and
    `pkgcache_low_speed_limit` options have priority over these
    environment variables, if they are set.
-   `R_PKG_CACHE_DIR` is used for the cache directory, if set.
    (Otherwise `rappdirs::user_cache_dir()` is used, see also
    `meta_cache_summary()` and `pkg_cache_summary()`).

## Using pkgcache in CRAN packages

If you use pkgcache in your CRAN package, please make sure that

-   you don’t use pkgcache in your examples, and
-   you set the `R_USER_CACHE_DIR` environment variable to a temporary
    directory (e.g. via `tempfile()`) during test cases. See the
    `tests/testthat/setup.R` file in pkgcache for an example.

This is to make sure that pkgcache does not modify the user’s files
while running `R CMD check`.

## Code of Conduct

Please note that the pkgcache project is released with a [Contributor
Code of
Conduct](https://github.com/r-lib/pkgcache/blob/main/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## License

MIT (c) [RStudio Inc](https://www.rstudio.com/)
