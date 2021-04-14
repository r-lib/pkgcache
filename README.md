
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
status](https://codecov.io/gh/r-lib/pkgcache/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/pkgcache?branch=master)
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
#> # A tibble: 35,272 x 33
#>    package title version depends suggests built imports archs repodir
#>    <chr>   <chr> <chr>   <chr>   <chr>    <chr> <chr>   <chr> <chr>  
#>  1 A3      "Acc… 1.0.0   R (>= … randomF… R 3.… <NA>    <NA>  bin/ma…
#>  2 aaSEA   Amin… 1.1.0   R(>= 3… knitr, … R 3.… "DT(>=… <NA>  bin/ma…
#>  3 ABACUS  "App… 1.0.0   R (>= … rmarkdo… R 3.… ggplot… <NA>  bin/ma…
#>  4 abbyyR  Acce… 0.5.5   R (>= … testtha… R 3.… httr, … <NA>  bin/ma…
#>  5 abc.da… Data… 1.0     R (>= … <NA>     R 3.… <NA>    <NA>  bin/ma…
#>  6 ABC.RAP Arra… 0.9.0   R (>= … knitr, … R 3.… graphi… <NA>  bin/ma…
#>  7 abc     Tool… 2.1     R (>= … <NA>     R 3.… <NA>    <NA>  bin/ma…
#>  8 abcADM  Fit … 1.0     <NA>    <NA>     R 3.… Rcpp (… abcA… bin/ma…
#>  9 ABCana… Comp… 1.2.1   R (>= … <NA>     R 3.… plotrix <NA>  bin/ma…
#> 10 abcdeF… "ABC… 0.4     Rglpk,… LIM,syb… R 3.… <NA>    <NA>  bin/ma…
#> # … with 35,262 more rows, and 24 more variables: platform <chr>,
#> #   rversion <chr>, needscompilation <chr>, priority <chr>, ref <chr>,
#> #   type <chr>, direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   published <dttm>, deps <list>, license <chr>, md5sum <chr>,
#> #   linkingto <chr>, enhances <chr>, license_restricts_use <chr>,
#> #   os_type <chr>, license_is_foss <chr>, path <chr>
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
#> [1] "/Users/gaborcsardi/Library/Caches/R-pkg/pkg"
#> 
#> $files
#> [1] 677
#> 
#> $size
#> [1] 608180353
```

``` r
pkg_cache_list()
#> # A tibble: 677 x 10
#>    fullpath path  package url   etag  sha256 built version platform
#>    <chr>    <chr> <chr>   <chr> <chr> <chr>  <int> <chr>   <chr>   
#>  1 /Users/… src/… crayon  <NA>  <NA>  84be6…     0 <NA>    <NA>    
#>  2 /Users/… bin/… proces… http… "\"3… ce84a…    NA 3.4.1   macos   
#>  3 /Users/… bin/… R6      http… "\"d… 4cfe3…    NA 2.4.0   macos   
#>  4 /Users/… bin/… ps      http… "\"3… 63add…    NA 1.3.0   macos   
#>  5 /Users/… src/… callr   <NA>  <NA>  df85a…     0 <NA>    <NA>    
#>  6 /Users/… bin/… callr   http… "\"5… 1a5e3…    NA 3.3.1   macos   
#>  7 /Users/… bin/… assert… http… "\"d… 103f9…    NA 0.2.1   macos   
#>  8 /Users/… bin/… backpo… http… "\"d… 344bb…    NA 1.1.4   macos   
#>  9 /Users/… bin/… crayon  http… "\"b… 2ac4f…    NA 1.3.4   macos   
#> 10 /Users/… bin/… desc    http… "\"4… 196ce…    NA 1.2.0   macos   
#> # … with 667 more rows, and 1 more variable: vignettes <chr>
```

``` r
pkg_cache_find(package = "dplyr")
#> # A tibble: 4 x 10
#>   fullpath path  package url   etag  sha256 built version platform
#>   <chr>    <chr> <chr>   <chr> <chr> <chr>  <int> <chr>   <chr>   
#> 1 /Users/… bin/… dplyr   http… "\"6… e2594…    NA 0.8.3   macos   
#> 2 /Users/… src/… dplyr   <NA>  <NA>  134b1…     0 <NA>    <NA>    
#> 3 /Users/… src/… dplyr   <NA>  <NA>  b357f…     0 <NA>    <NA>    
#> 4 /Users/… src/… dplyr   <NA>  <NA>  81767…     0 <NA>    <NA>    
#> # … with 1 more variable: vignettes <chr>
```

`pkg_cache_add_file()` can be used to add a file,
`pkg_cache_delete_files()` to remove files, `pkg_cache_get_files()` to
copy files out of the cache.

The `package_cache` class provides a finer API.

## Bioconductor support

Both the metadata cache and the package cache support Bioconductor by
default, automatically. See the `BioC_mirror` option and the
`R_BIOC_MIRROR` and `R_BIOC_VERSION` environment variables below to
configure pkgcache’s Bioconductor support.

## Package Options

  - The `BioC_mirror` option can be used to select a Bioconductor
    mirror. This takes priority over the `R_BIOC_MIRROR` environment
    variable.
  - `pkgcache_timeout` is the HTTP timeout for all downloads. It is in
    seconds, and the limit for downloading the whole file. Defaults to
    3600, one hour. It corresponds to the [`TIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html).
  - `pkgcache_connecttimeout` is the HTTP timeout for the connection
    phase. It is in seconds and defaults to 30 seconds. It corresponds
    to the [`CONNECTTIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html).
  - `pkgcache_low_speed_limit` and `pkgcache_low_speed_time` are used
    for a more sensible HTTP timeout. If the download speed is less than
    `pkgcache_low_speed_limit` bytes per second for at least
    `pkgcache_low_speed_time` seconds, the download errors. They
    correspond to the
    [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
    and
    [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
    curl options.

## Package environment variables

  - The `R_BIOC_VERSION` environment variable can be used to override
    the default Bioconductor version detection and force a given
    version. E.g. this can be used to force the development version of
    Bioconductor.
  - The `R_BIOC_MIRROR` environment variable can be used to select a
    Bioconductor mirror. The `BioC_mirror` option takes priority over
    this, if set.
  - `PKGCACHE_TIMEOUT` is the HTTP timeout for all downloads. It is in
    seconds, and the limit for downloading the whole file. Defaults to
    3600, one hour. It corresponds to the [`TIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html). The
    `pkgcache_timeout` option has priority over this, if set.
  - `PKGCACHE_CONNECTTIMEOUT` is the HTTP timeout for the connection
    phase. It is in seconds and defaults to 30 seconds. It corresponds
    to the [`CONNECTTIMEOUT` libcurl
    option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html).
    The `pkgcache_connecttimeout` option takes precedence over this, if
    set.
  - `PKGCACHE_LOW_SPEED_LIMIT` and `PKGCACHE_LOW_SPEED_TIME` are used
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
  - `R_PKG_CACHE_DIR` is used for the cache directory, if set.
    (Otherwise `rappdirs::user_cache_dir()` is used, see also
    `meta_cache_summary()` and `pkg_cache_summary()`).

## Code of Conduct

Please note that the ‘pkgcache’ project is released with a [Contributor
Code of
Conduct](https://github.com/r-lib/pkgcache/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## License

MIT © [RStudio Inc](https://www.rstudio.com/)
