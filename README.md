
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgcache

> Cache CRAN-like metadata and package
files

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgcache)](https://cran.r-project.org/package=pkgcache)
[![Travis build
status](https://travis-ci.org/r-lib/pkgcache.svg?branch=master)](https://travis-ci.org/r-lib/pkgcache)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/pkgcache?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/pkgcache)
[![Coverage
status](https://codecov.io/gh/r-lib/pkgcache/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/pkgcache?branch=master)

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
#> # A tibble: 30,246 x 29
#>    package title version depends suggests built imports archs repodir
#>    <chr>   <chr> <chr>   <chr>   <chr>    <chr> <chr>   <chr> <chr>  
#>  1 A3      "Acc… 1.0.0   R (>= … randomF… R 3.… <NA>    <NA>  bin/ma…
#>  2 abbyyR  Acce… 0.5.4   R (>= … testtha… R 3.… httr, … <NA>  bin/ma…
#>  3 abc.da… Data… 1.0     R (>= … <NA>     R 3.… <NA>    <NA>  bin/ma…
#>  4 ABC.RAP Arra… 0.9.0   R (>= … knitr, … R 3.… graphi… <NA>  bin/ma…
#>  5 abc     Tool… 2.1     R (>= … <NA>     R 3.… <NA>    <NA>  bin/ma…
#>  6 ABCana… Comp… 1.2.1   R (>= … <NA>     R 3.… plotrix <NA>  bin/ma…
#>  7 abcdeF… "ABC… 0.4     Rglpk,… LIM,syb… R 3.… <NA>    <NA>  bin/ma…
#>  8 ABCopt… Impl… 0.15.0  <NA>    testtha… R 3.… Rcpp, … ABCo… bin/ma…
#>  9 ABCp2   Appr… 1.2     MASS    <NA>     R 3.… <NA>    <NA>  bin/ma…
#> 10 abcrf   Appr… 1.7.1   R(>= 3… <NA>     R 3.… "readr… abcr… bin/ma…
#> # ... with 30,236 more rows, and 20 more variables: platform <chr>,
#> #   rversion <chr>, needscompilation <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, deps <list>, license <chr>, linkingto <chr>,
#> #   enhances <chr>, license_restricts_use <chr>, os_type <chr>,
#> #   priority <chr>, license_is_foss <chr>, md5sum <chr>, path <chr>
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
#> [1] "/Users/gaborcsardi/Library/Caches/R-pkg"
#> 
#> $files
#> [1] 40
#> 
#> $size
#> [1] 46040994
```

``` r
pkg_cache_list()
#> # A tibble: 40 x 8
#>    fullpath     path     package  url       etag   md5    version platform
#>    <chr>        <chr>    <chr>    <chr>     <chr>  <chr>  <chr>   <chr>   
#>  1 /Users/gabo… bin/mac… pkgconf… https://… "\"44… 32266… 2.0.2   macos   
#>  2 /Users/gabo… src/con… pkgconf… https://… "\"17… 65742… 2.0.2   source  
#>  3 /Users/gabo… bin/mac… bindr    https://… "\"3b… e7704… 0.1.1   macos   
#>  4 /Users/gabo… bin/mac… assertt… https://… "\"cc… d8cc7… 0.2.0   macos   
#>  5 /Users/gabo… bin/mac… bindrcpp https://… "\"5c… 0a457… 0.2.2   macos   
#>  6 /Users/gabo… bin/mac… cli      https://… "\"4d… 8591a… 1.0.0   macos   
#>  7 /Users/gabo… bin/mac… crayon   https://… "\"b7… aea16… 1.3.4   macos   
#>  8 /Users/gabo… bin/mac… pillar   https://… "\"24… 90119… 1.3.0   macos   
#>  9 /Users/gabo… bin/mac… glue     https://… "\"16… ba05c… 1.3.0   macos   
#> 10 /Users/gabo… bin/mac… magrittr https://… "\"25… 97f02… 1.5     macos   
#> # ... with 30 more rows
```

``` r
pkg_cache_find(package = "dplyr")
#> # A tibble: 2 x 8
#>   fullpath      path     package url        etag   md5    version platform
#> * <chr>         <chr>    <chr>   <chr>      <chr>  <chr>  <chr>   <chr>   
#> 1 /Users/gabor… bin/mac… dplyr   https://c… "\"56… 004df… 0.7.6   macos   
#> 2 /Users/gabor… src/con… dplyr   https://c… "\"ad… cc51d… 0.7.6   source
```

`pkg_cache_add_file()` can be used to add a file,
`pkg_cache_delete_files()` to remove files, `pkg_cache_get_files()` to
copy files out of the cache.

The `package_cache` class provides a finer API.

## Code of Conduct

Please note that the ‘pkgcache’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.

## License

MIT © [RStudio Inc](https://rstudio.com)
