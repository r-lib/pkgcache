# Show the status of CRAN-like repositories

It checks the status of the configured or supplied repositories, for the
specified platforms and R versions.

## Usage

``` r
repo_status(
  platforms = default_platforms(),
  r_version = getRversion(),
  bioc = TRUE,
  cran_mirror = default_cran_mirror()
)
```

## Arguments

- platforms:

  Platforms to use, default is
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md).

- r_version:

  R version(s) to use, the default is the current R version, via
  [`getRversion()`](https://rdrr.io/r/base/numeric_version.html).

- bioc:

  Whether to add the Bioconductor repositories. If you already
  configured them via `options(repos)`, then you can set this to
  `FALSE`. See
  [`bioc_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  for the details about how pkgcache handles Bioconductor repositories.

- cran_mirror:

  The CRAN mirror to use, see
  [`default_cran_mirror()`](https://r-lib.github.io/pkgcache/dev/reference/default_cran_mirror.md).

## Value

A data frame that has a row for every repository, on every queried
platform and R version. It has these columns:

- `name`: the name of the repository. This comes from the names of the
  configured repositories in `options("repos")`, or added by pkgcache.
  It is typically `CRAN` for CRAN, and the current Bioconductor
  repositories are `BioCsoft`, `BioCann`, `BioCexp`, `BioCworkflows`,
  `BioCbooks`.

- `url`: base URL of the repository.

- `bioc_version`: Bioconductor version, or `NA` for non-Bioconductor
  repositories.

- `username`: Included if at least one repository is authenticated.
  `NA_character_` for repositories without authentication. See
  [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md).

- `has_password`: `TRUE` is the function could retrieve the password for
  the authenticated repository. It is `NA` for repositories without
  authentication. This column is included only if at least one
  repository has authentication. See
  [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md).

- `platform`: platform, see
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  for possible values.

- `path`: the path to the packages within the base URL, for a given
  platform and R version.

- `r_version`: R version, one of the specified R versions.

- `ok`: Logical flag, whether the repository contains a metadata file
  for the given platform and R version.

- `ping`: HTTP response time of the repository in seconds. If the `ok`
  column is `FALSE`, then this columns in `NA`.

- `error`: the error object if the HTTP query failed for this
  repository, platform and R version.

## Details

The returned data frame has a
[`summary()`](https://rdrr.io/r/base/summary.html) method, which shows
the same information is a concise table. See examples below.

## See also

Other repository functions:
[`repo_get()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)

## Examples

``` r
repo_status()
#> # A data frame: 7 × 10
#>   name   url   type  bioc_version platform path  r_version ok      ping
#>   <chr>  <chr> <chr> <chr>        <chr>    <chr> <chr>     <lgl>  <dbl>
#> 1 RSPM   http… cran… NA           source   src/… 4.5       TRUE  0.333 
#> 2 CRAN   http… cran  NA           source   src/… 4.5       TRUE  0.0432
#> 3 BioCs… http… bioc  3.22         source   src/… 4.5       TRUE  0.104 
#> 4 BioCa… http… bioc  3.22         source   src/… 4.5       TRUE  0.308 
#> 5 BioCe… http… bioc  3.22         source   src/… 4.5       TRUE  0.308 
#> 6 BioCw… http… bioc  3.22         source   src/… 4.5       TRUE  0.308 
#> 7 BioCb… http… bioc  3.22         source   src/… 4.5       TRUE  0.113 
#> # ℹ 1 more variable: error <list>
rst <- repo_status(
  platforms = c("windows", "macos"),
  r_version = c("4.0", "4.1")
)
summary(rst)
#> Repository summary:                               i386+x86_64-w64-mingw32 x86_64-apple-darwin17.0          
#> RSPM          @ packagemanager.posit.co (R 4.0)              ✔                       ✔              (349ms)
#> CRAN          @ cran.rstudio.com        (R 4.0)              ✔                       ✔              (71ms )
#> RSPM          @ packagemanager.posit.co (R 4.1)              ✔                       ✔              (358ms)
#> CRAN          @ cran.rstudio.com        (R 4.1)              ✔                       ✔              (75ms )
#> BioCsoft      @ bioconductor.org        (R 4.0)              ✔                       ✔              (602ms)
#> BioCann       @ bioconductor.org        (R 4.0)              ✔                       ✔              (674ms)
#> BioCexp       @ bioconductor.org        (R 4.0)              ✔                       ✔              (720ms)
#> BioCworkflows @ bioconductor.org        (R 4.0)              ✔                       ✔              (782ms)
#> BioCbooks     @ bioconductor.org        (R 4.0)              ✔                       ✔              (460ms)
#> BioCsoft      @ bioconductor.org        (R 4.1)              ✔                       ✔              (720ms)
#> BioCann       @ bioconductor.org        (R 4.1)              ✔                       ✔              (702ms)
#> BioCexp       @ bioconductor.org        (R 4.1)              ✔                       ✔              (775ms)
#> BioCworkflows @ bioconductor.org        (R 4.1)              ✔                       ✔              (823ms)
#> BioCbooks     @ bioconductor.org        (R 4.1)              ✔                       ✔              (282ms)
```
