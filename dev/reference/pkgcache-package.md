# Cache for package data and metadata

Metadata and package cache for CRAN-like repositories. This is a utility
package to be used by package management tools that want to take
advantage of caching.

## Details

Metadata and package cache for CRAN-like repositories. This is a utility
package to be used by package management tools that want to take
advantage of caching.

### Installation

You can install the released version of pkgcache from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("pkgcache")

If you need the development version, you can install it from
[GitHub](https://github.com) with:

    pak::pak("r-lib/pkgcache")

### Metadata cache

[`meta_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
lists all packages in the metadata cache. It includes Bioconductor
package, and all versions (i.e. both binary and source) of the packages
for the current platform and R version.

(We load the pillar package, because it makes the pkgcache data frames
print nicer, similarly to tibbles.)

    library(pkgcache)
    library(pillar)
    meta_cache_list()
    #> # A data frame: 53,991 x 33
    #>    package     version depends imports license md5sum sha256sum needscompilation
    #>    <chr>       <chr>   <chr>   <chr>   <chr>   <chr>  <chr>     <chr>
    #>  1 AATtools    0.0.3   R (>= ~ "magri~ GPL-3   77b8a~ "\n     ~ no
    #>  2 ABACUS      1.0.0   R (>= ~ "ggplo~ GPL-3   f6450~ "\n     ~ no
    #>  3 ABC.RAP     0.9.0   R (>= ~ "graph~ GPL-3   0dd81~ "\n     ~ no
    #>  4 ABCDscores  6.1.0   R (>= ~ "chk, ~ GPL (>~ 0de2a~ "\n     ~ no
    #>  5 ABCanalysis 1.2.1   R (>= ~ "plotr~ GPL-3   fb77c~ "\n     ~ no
    #>  6 ABCoptim    0.15.0  <NA>    "Rcpp,~ MIT + ~ 0487b~ "\n     ~ yes
    #>  7 ABHgenotyp~ 1.0.1   <NA>    "ggplo~ GPL-3   2b2aa~ "\n     ~ no
    #>  8 ABM         0.4.3   <NA>    "R6, R~ GPL (>~ 338a6~ "\n     ~ yes
    #>  9 ABPS        0.3     <NA>    "kernl~ GPL (>~ 54a9e~ "\n     ~ no
    #> 10 ABRSQOL     1.0.0   R (>= ~  <NA>   MIT + ~ 2fb4a~ "\n     ~ no
    #> # i 53,981 more rows
    #> # i 25 more variables: suggests <chr>, linkingto <chr>, archs <chr>,
    #> #   enhances <chr>, priority <chr>, os_type <chr>, license_is_foss <chr>,
    #> #   license_restricts_use <chr>, repodir <chr>, rversion <chr>, platform <chr>,
    #> #   ref <chr>, type <chr>, direct <lgl>, status <chr>, target <chr>,
    #> #   mirror <chr>, sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
    #> #   built <chr>, published <dttm>, deps <list>, path <chr>

[`meta_cache_deps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
and
[`meta_cache_revdeps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
can be used to look up dependencies and reverse dependencies.

The metadata is updated automatically if it is older than seven days,
and it can also be updated manually with
[`meta_cache_update()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md).

See the `cranlike_metadata_cache` R6 class for a lower level API, and
more control.

### Package cache

Package management tools may use the `pkg_cache_*` functions and in
particular the `package_cache` class, to make use of local caching of
package files.

The `pkg_cache_*` API is high level, and uses a user level cache:

    pkg_cache_summary()
    #> $cachepath
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/pkg"
    #>
    #> $files
    #> [1] 544
    #>
    #> $size
    #> [1] 501117315

    pkg_cache_list()
    #> # A data frame: 544 x 11
    #>    fullpath    path  package url   etag  sha256 version platform built vignettes
    #>    <chr>       <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>    <chr> <chr>
    #>  1 /Users/gab~ bin/~ cli     http~ "\"1~ b747b~ 3.6.5   aarch64~ <NA>  <NA>
    #>  2 /Users/gab~ bin/~ brio    http~ "\"c~ 08165~ 1.1.5   aarch64~ <NA>  <NA>
    #>  3 /Users/gab~ bin/~ askpass http~ "\"6~ 8c6b9~ 1.2.1   aarch64~ <NA>  <NA>
    #>  4 /Users/gab~ bin/~ glue    http~ "\"2~ f0b34~ 1.8.0   aarch64~ <NA>  <NA>
    #>  5 /Users/gab~ bin/~ crayon  http~ "\"2~ 77feb~ 1.5.3   aarch64~ <NA>  <NA>
    #>  6 /Users/gab~ bin/~ covr    http~ "\"5~ 55f0f~ 3.6.5   aarch64~ <NA>  <NA>
    #>  7 /Users/gab~ bin/~ callr   http~ "\"6~ 7cd55~ 3.7.6   aarch64~ <NA>  <NA>
    #>  8 /Users/gab~ bin/~ evalua~ http~ "\"1~ 307ea~ 1.0.5   aarch64~ <NA>  <NA>
    #>  9 /Users/gab~ bin/~ curl    http~ "\"1~ 15871~ 7.0.0   aarch64~ <NA>  <NA>
    #> 10 /Users/gab~ bin/~ pkgbui~ http~ "\"3~ d874a~ 1.4.8   aarch64~ <NA>  <NA>
    #> # i 534 more rows
    #> # i 1 more variable: rversion <chr>

    pkg_cache_find(package = "dplyr")
    #> # A data frame: 3 x 11
    #>   fullpath     path  package url   etag  sha256 version platform built vignettes
    #>   <chr>        <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>    <chr> <chr>
    #> 1 /Users/gabo~ src/~ dplyr   http~ "\"e~ a82c2~ 1.2.0   source   <NA>  <NA>
    #> 2 /Users/gabo~ src/~ dplyr   <NA>   <NA> <NA>   1.2.0   aarch64~ TRUE  FALSE
    #> 3 /Users/gabo~ bin/~ dplyr   http~ "\"1~ 64938~ 1.2.1   aarch64~ <NA>  <NA>
    #> # i 1 more variable: rversion <chr>

[`pkg_cache_add_file()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
can be used to add a file,
[`pkg_cache_delete_files()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
to remove files, `pkg_cache_get_files()` to copy files out of the cache.

The `package_cache` class provides a finer API.

### Installed packages

pkgcache contains a very fast DCF parser to parse `PACKAGES*` files, or
the `DESCRIPTION` files in installed packages.
[`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
parses all fields from `PACKAGES`, `PACKAGES.gz` or `PACKAGES.rds`
files.
[`parse_installed()`](https://r-lib.github.io/pkgcache/dev/reference/parse_installed.md)
reads *all* metadata from packages installed into a library:

    parse_installed()
    #> # A data frame: 281 x 106
    #>    Package     Title    Version `Authors@R` Description License URL   BugReports
    #>    <chr>       <chr>    <chr>   <chr>       <chr>       <chr>   <chr> <chr>
    #>  1 pkgcache    "Cache ~ 2.2.5.~ "c(\n    p~ "Metadata ~ MIT + ~ "htt~ https://g~
    #>  2 BiocManager "Access~ 1.30.27 "c(\n    p~ "A conveni~ Artist~ "htt~ https://g~
    #>  3 DBI         "R Data~ 1.3.0   "c(\n    p~ "A databas~ LGPL (~ "htt~ https://g~
    #>  4 Formula     "Extend~ 1.2-5   "c(person(~ "Infrastru~ GPL-2 ~  <NA> <NA>
    #>  5 GPArotation "Gradie~ 2025.3~ "c( \n\tpe~ "Gradient ~ GPL (>~ "htt~ <NA>
    #>  6 Hmisc       "Harrel~ 5.2-5   "\n    c(p~ "Contains ~ GPL (>~ "htt~ <NA>
    #>  7 PlotTools   "Extend~ 0.4.0   "c(person(~ "Annotate ~ GPL (>~ "htt~ https://g~
    #>  8 R.cache     "Fast a~ 0.17.0  "c(person(~ "Memoizati~ LGPL (~ "htt~ https://g~
    #>  9 R.methodsS3 "S3 Met~ 1.8.2   "c(person(~ "Methods t~ LGPL (~ "htt~ https://g~
    #> 10 R.oo        "R Obje~ 1.27.1  "c(person(~ "Methods a~ LGPL (~ "htt~ https://g~
    #> # i 271 more rows
    #> # i 98 more variables: Depends <chr>, Imports <chr>, Suggests <chr>,
    #> #   `Config/Needs/website` <chr>, `Config/testthat/edition` <chr>,
    #> #   `Config/usethis/last-upkeep` <chr>, Encoding <chr>, Language <chr>,
    #> #   Roxygen <chr>, RoxygenNote <chr>, Author <chr>, Maintainer <chr>,
    #> #   Built <chr>, LibPath <chr>, VignetteBuilder <chr>, NeedsCompilation <chr>,
    #> #   Packaged <chr>, Repository <chr>, `Date/Publication` <chr>, ...

### Bioconductor support

Both the metadata cache and the package cache support Bioconductor by
default, automatically. See the `BioC_mirror` option and the
`R_BIOC_MIRROR` and `R_BIOC_VERSION` environment variables below to
configure Bioconductor support.

### Package Options

- The `BioC_mirror` option can be used to select a Bioconductor mirror.
  This takes priority over the `R_BIOC_MIRROR` environment variable.

- You can use the `pkg.current_platform` option to set the platform
  string for the current platform for the
  [`current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  function. This is useful if pkgcache didn’t detect the platform
  correctly. Alternatively, you can use the `PKG_CURRENT_PLATFORM`
  environment variable. The option takes priority.

- `pkgcache_timeout` is the HTTP timeout for all downloads. It is in
  seconds, and the limit for downloading the whole file. Defaults to
  3600, one hour. It corresponds to the [`TIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html).

- `pkgcache_connecttimeout` is the HTTP timeout for the connection
  phase. It is in seconds and defaults to 30 seconds. It corresponds to
  the [`CONNECTTIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html).

- `pkgcache_low_speed_limit` and `pkgcache_low_speed_time` are used for
  a more sensible HTTP timeout. If the download speed is less than
  `pkgcache_low_speed_limit` bytes per second for at least
  `pkgcache_low_speed_time` seconds, the download errors. They
  correspond to the
  [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
  and
  [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
  curl options.

- `pkgcache_http_version` selects the HTTP version to use for HTTP
  requests. It corresponds to the [`HTTP_VERSION` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_HTTP_VERSION.html), so
  e.g. `2` forces HTTP/1.1 and `0` lets libcurl choose. It defaults to
  HTTP/1.1, because HTTP/2 has caused transport-level failures with some
  client and server combinations.

### Package environment variables

- The `R_BIOC_VERSION` environment variable can be used to override the
  default Bioconductor version detection and force a given version. E.g.
  this can be used to force the development version of Bioconductor.

- The `R_BIOC_MIRROR` environment variable can be used to select a
  Bioconductor mirror. The `BioC_mirror` option takes priority over
  this, if set.

- You can use the `PKG_CURRENT_PLATFORM` environment variable to set the
  platform string for the current platform for the
  [`current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  function. This is useful if pkgcache didn’t detect the platform
  correctly. Alternatively, you can use the `pkg.current_platofrm`
  option, which takes. priority over the environment variable.

- `PKGCACHE_PPM_REPO` is the name of the Posit Package Manager
  repository to use. Defaults to `"cran"`.

- `PKGCACHE_PPM_URL` is the base URL of the Posit Package Manager
  instance to use. It defaults to the URL of the Posit Public Package
  Manager instance at <https://packagemanager.posit.co/client/#/>.

- `PKGCACHE_TIMEOUT` is the HTTP timeout for all downloads. It is in
  seconds, and the limit for downloading the whole file. Defaults to
  3600, one hour. It corresponds to the [`TIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html). The
  `pkgcache_timeout` option has priority over this, if set.

- `PKGCACHE_CONNECTTIMEOUT` is the HTTP timeout for the connection
  phase. It is in seconds and defaults to 30 seconds. It corresponds to
  the [`CONNECTTIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html). The
  `pkgcache_connecttimeout` option takes precedence over this, if set.

- `PKGCACHE_LOW_SPEED_LIMIT` and `PKGCACHE_LOW_SPEED_TIME` are used for
  a more sensible HTTP timeout. If the download speed is less than
  `PKGCACHE_LOW_SPEED_LIMIT` bytes per second for at least
  `PKGCACHE_LOW_SPEED_TIME` seconds, the download errors. They
  correspond to the
  [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
  and
  [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
  curl options. The `pkgcache_low_speed_time` and
  `pkgcache_low_speed_limit` options have priority over these
  environment variables, if they are set.

- `PKGCACHE_HTTP_VERSION` selects the HTTP version to use for HTTP
  requests. It corresponds to the [`HTTP_VERSION` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_HTTP_VERSION.html), so
  e.g. `2` forces HTTP/1.1 and `0` lets libcurl choose. It defaults to
  HTTP/1.1, because HTTP/2 has caused transport-level failures with some
  client and server combinations. The `pkgcache_http_version` option has
  priority over this, if set.

- `R_PKG_CACHE_DIR` is used for the cache directory, if set. (Otherwise
  `tools::R_user_dir("pkgcache", "cache")` is used, see also
  [`meta_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  and
  [`pkg_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)).

### Using pkgcache in CRAN packages

If you use pkgcache in your CRAN package, please make sure that

- you don’t use pkgcache in your examples, and

- you set the `R_USER_CACHE_DIR` environment variable to a temporary
  directory (e.g. via
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html)) during test
  cases. See the `tests/testthat/setup.R` file in pkgcache for an
  example.

This is to make sure that pkgcache does not modify the user’s files
while running `R CMD check`.

### Code of Conduct

Please note that the pkgcache project is released with a [Contributor
Code of Conduct](https://r-lib.github.io/pkgcache/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

### License

MIT (c) [Posit Software, PBC](https://posit.co)

## See also

Useful links:

- <https://r-lib.github.io/pkgcache/>

- <https://github.com/r-lib/pkgcache>

- Report bugs at <https://github.com/r-lib/pkgcache/issues>

## Author

**Maintainer**: Gábor Csárdi <csardi.gabor@gmail.com>

Authors:

- Gábor Csárdi <csardi.gabor@gmail.com>

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
