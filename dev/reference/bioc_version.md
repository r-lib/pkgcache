# Query Bioconductor version information

Various helper functions to deal with Bioconductor repositories. See
<https://www.bioconductor.org/> for more information on Bioconductor.

## Usage

``` r
bioc_version(r_version = getRversion(), forget = FALSE)

bioc_version_map(forget = FALSE)

bioc_devel_version(forget = FALSE)

bioc_release_version(forget = FALSE)

bioc_repos(bioc_version = "auto", forget = FALSE)
```

## Arguments

- r_version:

  The R version number to match.

- forget:

  Use `TRUE` to avoid caching the Bioconductor mapping.

- bioc_version:

  Bioconductor version string or `package_version` object, or the string
  `"auto"` to use the one matching the current R version.

## Value

`bioc_version()` returns a
[package_version](https://rdrr.io/r/base/numeric_version.html) object.

`bioc_version_map()` returns a data frame with columns:

- `bioc_version`:
  [package_version](https://rdrr.io/r/base/numeric_version.html) object,
  Bioconductor versions.

- `r_version`:
  [package_version](https://rdrr.io/r/base/numeric_version.html) object,
  the matching R versions.

- `bioc_status`: factor, with levels: `out-of-date`, `release`, `devel`,
  `future`.

`bioc_devel_version()` returns a
[package_version](https://rdrr.io/r/base/numeric_version.html) object.

`bioc_release_version()` returns a
[package_version](https://rdrr.io/r/base/numeric_version.html) object.

`bioc_repos()` returns a named character vector.

## Details

`bioc_version()` queries the matching Bioconductor version for an R
version, defaulting to the current R version

`bioc_version_map()` returns the current mapping between R versions and
Bioconductor versions.

`bioc_devel_version()` returns the version number of the current
Bioconductor devel version.

`bioc_release_version()` returns the version number of the current
Bioconductor release.

`bioc_repos()` returns the Bioconductor repository URLs.

See the `BioC_mirror` option and the `R_BIOC_MIRROR` and
`R_BIOC_VERSION` environment variables in the
[pkgcache](https://r-lib.github.io/pkgcache/dev/reference/pkgcache-package.md)
manual page. They can be used to customize the desired Bioconductor
version.

## Examples

``` r
bioc_version()
#> [1] ‘3.22’
bioc_version("4.0")
#> [1] ‘3.12’
bioc_version("4.1")
#> [1] ‘3.14’
bioc_version_map()
#> # A data frame: 44 × 3
#>    bioc_version r_version  bioc_status
#>    <pckg_vrs>   <pckg_vrs> <fct>      
#>  1 1.6          2.1        out-of-date
#>  2 1.7          2.2        out-of-date
#>  3 1.8          2.3        out-of-date
#>  4 1.9          2.4        out-of-date
#>  5 2.0          2.5        out-of-date
#>  6 2.1          2.6        out-of-date
#>  7 2.2          2.7        out-of-date
#>  8 2.3          2.8        out-of-date
#>  9 2.4          2.9        out-of-date
#> 10 2.5          2.10       out-of-date
#> # ℹ 34 more rows
bioc_devel_version()
#> [1] ‘3.23’
bioc_release_version()
#> [1] ‘3.22’
bioc_repos()
#>                                                 BioCsoft 
#>            "https://bioconductor.org/packages/3.22/bioc" 
#>                                                  BioCann 
#> "https://bioconductor.org/packages/3.22/data/annotation" 
#>                                                  BioCexp 
#> "https://bioconductor.org/packages/3.22/data/experiment" 
#>                                            BioCworkflows 
#>       "https://bioconductor.org/packages/3.22/workflows" 
#>                                                BioCbooks 
#>           "https://bioconductor.org/packages/3.22/books" 
```
