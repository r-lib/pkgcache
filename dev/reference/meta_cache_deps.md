# Query CRAN(like) package data

It uses CRAN and BioConductor packages, for the current platform and R
version, from the default repositories.

## Usage

``` r
meta_cache_deps(packages, dependencies = NA, recursive = TRUE)

meta_cache_revdeps(packages, dependencies = NA, recursive = TRUE)

meta_cache_update()

meta_cache_list(packages = NULL)

meta_cache_cleanup(force = FALSE)

meta_cache_summary()
```

## Arguments

- packages:

  Packages to query.

- dependencies:

  Dependency types to query. See the `dependencies` parameter of
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

- recursive:

  Whether to query recursive dependencies.

- force:

  Whether to force cleanup without asking the user.

## Value

A data frame of the dependencies. For `meta_cache_deps()` and
`meta_cache_revdeps()` it includes the queried `packages` as well.

## Details

`meta_cache_list()` lists all packages.

`meta_cache_update()` updates all metadata. Note that metadata is
automatically updated if it is older than seven days.

`meta_cache_deps()` queries packages dependencies.

`meta_cache_revdeps()` queries reverse package dependencies.

`meta_cache_summary()` lists data about the cache, including its
location and size.

`meta_cache_cleanup()` deletes the cache files from the disk.

## Examples

``` r
meta_cache_list("pkgdown")
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> # A data frame: 2 × 32
#>   package version depends    imports  suggests needscompilation license
#> * <chr>   <chr>   <chr>      <chr>    <chr>    <chr>            <chr>  
#> 1 pkgdown 2.2.0   R (>= 4.1) "bslib … "covr, … NA               MIT + …
#> 2 pkgdown 2.2.0   R (>= 4.1) "bslib … "covr, … no               MIT + …
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>
meta_cache_deps("pkgdown", recursive = FALSE)
#> # A data frame: 42 × 32
#>    package    version depends imports suggests needscompilation license
#>  * <chr>      <chr>   <chr>   <chr>   <chr>    <chr>            <chr>  
#>  1 bslib      0.10.0  R (>= … "base6… "brand.… NA               MIT + …
#>  2 callr      3.7.6   R (>= … "proce… "asciic… NA               MIT + …
#>  3 cli        3.6.5   R (>= … "utils" "callr,… NA               MIT + …
#>  4 desc       1.4.3   R (>= … "cli, … "callr,… NA               MIT + …
#>  5 downlit    0.4.5   R (>= … "brio,… "covr, … NA               MIT + …
#>  6 fontaweso… 0.5.3   R (>= … "rlang… "covr, … NA               MIT + …
#>  7 fs         1.6.7   R (>= … "metho… "covr, … NA               MIT + …
#>  8 httr2      1.2.2   R (>= … "cli (… "askpas… NA               MIT + …
#>  9 jsonlite   2.0.0   methods  NA     "httr, … NA               MIT + …
#> 10 lifecycle  1.0.5   R (>= … "cli (… "covr, … NA               MIT + …
#> # ℹ 32 more rows
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>, …
meta_cache_revdeps("pkgdown", recursive = FALSE)
#> # A data frame: 10 × 32
#>    package    version depends imports suggests needscompilation license
#>  * <chr>      <chr>   <chr>   <chr>   <chr>    <chr>            <chr>  
#>  1 CALANGO    1.0.20  R (>= … "asser… "Annota… NA               GPL-2  
#>  2 devtools   2.4.6   R (>= … "cli (… "BiocMa… NA               MIT + …
#>  3 pkgdown    2.2.0   R (>= … "bslib… "covr, … NA               MIT + …
#>  4 pkgdown.o… 0.1.2   NA      "pkgdo… "knitr,… NA               MIT + …
#>  5 washr      1.0.1   NA      "desc … "knitr,… NA               GPL (>…
#>  6 CALANGO    1.0.20  R (>= … "asser… "Annota… no               GPL-2  
#>  7 devtools   2.5.0   R (>= … "cli (… "BiocMa… no               MIT + …
#>  8 pkgdown    2.2.0   R (>= … "bslib… "covr, … no               MIT + …
#>  9 pkgdown.o… 0.1.2   NA      "pkgdo… "knitr,… no               MIT + …
#> 10 washr      1.0.1   NA      "desc … "knitr,… no               GPL (>…
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>
```
