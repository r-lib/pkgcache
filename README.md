
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgcache

> Cache CRAN-like metadata and package files

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
#> ✔ Using cached package metadata
#> # A tibble: 32,705 x 33
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
#> # … with 32,695 more rows, and 24 more variables: platform <chr>,
#> #   rversion <chr>, needscompilation <chr>, priority <chr>, ref <chr>,
#> #   type <chr>, direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   published <dttm>, deps <list>, license <chr>, linkingto <chr>,
#> #   enhances <chr>, license_restricts_use <chr>, os_type <chr>,
#> #   license_is_foss <chr>, md5sum <chr>, path <chr>
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
#> [1] 12
#> 
#> $size
#> [1] 553116
```

``` r
pkg_cache_list()
#> # A tibble: 12 x 8
#>    fullpath       path        package   url  etag sha256    built vignettes
#>    <chr>          <glue>      <chr>   <int> <int> <chr>     <lgl> <chr>    
#>  1 /Users/gaborc… src/contri… cranca…    NA    NA 936fa23d… FALSE <NA>     
#>  2 /Users/gaborc… src/contri… cranca…    NA    NA 936fa23d… TRUE  FALSE    
#>  3 /Users/gaborc… src/contri… rcmdch…    NA    NA 6332ddc2… FALSE <NA>     
#>  4 /Users/gaborc… src/contri… rcmdch…    NA    NA 6332ddc2… TRUE  FALSE    
#>  5 /Users/gaborc… src/contri… revdep…    NA    NA e204aede… FALSE <NA>     
#>  6 /Users/gaborc… src/contri… revdep…    NA    NA e204aede… TRUE  FALSE    
#>  7 /Users/gaborc… src/contri… cranca…    NA    NA 936fa23d… FALSE <NA>     
#>  8 /Users/gaborc… src/contri… cranca…    NA    NA 936fa23d… TRUE  FALSE    
#>  9 /Users/gaborc… src/contri… revdep…    NA    NA e204aede… FALSE <NA>     
#> 10 /Users/gaborc… src/contri… revdep…    NA    NA e204aede… TRUE  FALSE    
#> 11 /Users/gaborc… src/contri… rcmdch…    NA    NA 6332ddc2… FALSE <NA>     
#> 12 /Users/gaborc… src/contri… rcmdch…    NA    NA 6332ddc2… TRUE  FALSE
```

``` r
pkg_cache_find(package = "dplyr")
#> # A tibble: 0 x 8
#> # … with 8 variables: fullpath <chr>, path <glue>, package <chr>,
#> #   url <int>, etag <int>, sha256 <chr>, built <lgl>, vignettes <chr>
```

`pkg_cache_add_file()` can be used to add a file,
`pkg_cache_delete_files()` to remove files, `pkg_cache_get_files()` to
copy files out of the cache.

The `package_cache` class provides a finer API.

## Bioconductor support

Both the metadata cache and the package cache support Bioconductor by
default, automatically. The following options and environment variables
can be used to configure pkgcache’s Bioconductor support:

Options:

  - The `BioC_mirror` option can be used to select a Bioconductor
    mirror. This takes priority over the `R_BIOC_MIRROR` environment
    variable.

Environment variables:

  - The `R_BIOC_VERSION` environment variable can be used to override
    the default Bioconductor version detection and force a given
    version. E.g. this can be used to force the development version of
    Bioconductor.
  - The `R_BIOC_MIRROR` environment variable can be used to select a
    Bioconductor mirror. The `BioC_mirror` option takes priority over
    this, if set.

## Code of Conduct

Please note that the ‘pkgcache’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.

## License

MIT © [RStudio Inc](https://rstudio.com)
