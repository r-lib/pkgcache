# Metadata cache for a CRAN-like repository

This is an R6 class that implements the metadata cache of a CRAN-like
repository. For a higher level interface, see the
[`meta_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md),
[`meta_cache_deps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md),
[`meta_cache_revdeps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
and
[`meta_cache_update()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
functions.

## Details

The cache has several layers:

- The data is stored inside the `cranlike_metadata_cache` object.

- It is also stored as an RDS file, in the session temporary directory.
  This ensures that the same data is used for all queries of a
  `cranlike_metadata_cache` object.

- It is stored in an RDS file in the user's cache directory.

- The downloaded raw `PACKAGES*` files are cached, together with HTTP
  ETags, to minimize downloads.

It has a synchronous and an asynchronous API.

## Usage

    cmc <- cranlike_metadata_cache$new(
      primary_path = NULL, replica_path = tempfile(),
      platforms = default_platforms(), r_version = getRversion(),
      bioc = TRUE, cran_mirror = default_cran_mirror(),
      repos = getOption("repos"),
      update_after = as.difftime(7, units = "days"))

    cmc$list(packages = NULL)
    cmc$async_list(packages = NULL)

    cmc$deps(packages, dependencies = NA, recursive = TRUE)
    cmc$async_deps(packages, dependencies = NA, recursive = TRUE)

    cmc$revdeps(packages, dependencies = NA, recursive = TRUE)
    cmc$async_revdeps(packages, dependencies = NA, recursive = TRUE)

    cmc$update()
    cmc$async_update()
    cmc$check_update()
    cmc$asnyc_check_update()

    cmc$summary()

    cmc$cleanup(force = FALSE)

## Arguments

- `primary_path`: Path of the primary, user level cache. Defaults to the
  user level cache directory of the machine.

- `replica_path`: Path of the replica. Defaults to a temporary directory
  within the session temporary directory.

- `platforms`: see
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  for possible values.

- `r_version`: R version to create the cache for.

- `bioc`: Whether to include BioConductor packages.

- `cran_mirror`: CRAN mirror to use, this takes precedence over `repos`.

- `repos`: Repositories to use.

- `update_after`: `difftime` object. Automatically update the cache if
  it gets older than this. Set it to `Inf` to avoid updates. Defaults to
  seven days.

- `packages`: Packages to query, character vector.

- `dependencies`: Which kind of dependencies to include. Works the same
  way as the `dependencies` argument of
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

- `recursive`: Whether to include recursive dependencies.

- `force`: Whether to force cleanup without asking the user.

## Details

`cranlike_metadata_cache$new()` creates a new cache object. Creation
does not trigger the population of the cache. It is only populated on
demand, when queries are executed against it. In your package, you may
want to create a cache instance in the `.onLoad()` function of the
package, and store it in the package namespace. As this is a cheap
operation, the package will still load fast, and then the package code
can refer to the common cache object.

`cmc$list()` lists all (or the specified) packages in the cache. It
returns a data frame, see the list of columns below.

`cmc$async_list()` is similar, but it is asynchronous, it returns a
`deferred` object.

`cmc$deps()` returns a data frame, with the (potentially recursive)
dependencies of `packages`.

`cmc$async_deps()` is the same, but it is asynchronous, it returns a
`deferred` object.

`cmc$revdeps()` returns a data frame, with the (potentially recursive)
reverse dependencies of `packages`.

`cmc$async_revdeps()` does the same, asynchronously, it returns an
`deferred` object.

`cmc$update()` updates the the metadata (as needed) in the cache, and
then returns a data frame with all packages, invisibly.

`cmc$async_update()` is similar, but it is asynchronous.

`cmc$check_update()` checks if the metadata is current, and if it is
not, it updates it.

`cmc$async_check_update()` is similar, but it is asynchronous.

`cmc$summary()` lists metadata about the cache, including its location
and size.

`cmc$cleanup()` deletes the cache files from the disk, and also from
memory.

## Columns

The metadata data frame contains all available versions (i.e. sources
and binaries) for all packages. It usually has the following columns,
some might be missing on some platforms.

- `package`: Package name.

- `title`: Package title.

- `version`: Package version.

- `depends`: `Depends` field from `DESCRIPTION`, or `NA_character_`.

- `suggests`: `Suggests` field from `DESCRIPTION`, or `NA_character_`.

- `built`: `Built` field from `DESCIPTION`, if a binary package, or
  `NA_character_`.

- `imports`: `Imports` field from `DESCRIPTION`, or `NA_character_`.

- `archs`: `Archs` entries from `PACKAGES` files. Might be missing.

- `repodir`: The directory of the file, inside the repository.

- `platform`: This is a character vector. See
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  for more about platform names. In practice each value of the
  `platform` column is either

  - `"source"` for source packages,

  - a platform string, e.g. `x86_64-apple-darwin17.0` for macOS packages
    compatible with macOS High Sierra or newer.

- `needscompilation`: Whether the package needs compilation.

- `type`: `bioc` or `cran` currently.

- `target`: The path of the package file inside the repository.

- `mirror`: URL of the CRAN/BioC mirror.

- `sources`: List column with URLs to one or more possible locations of
  the package file. For source CRAN packages, it contains URLs to the
  `Archive` directory as well, in case the package has been archived
  since the metadata was cached.

- `filesize`: Size of the file, if known, in bytes, or `NA_integer_`.

- `sha256`: The SHA256 hash of the file, if known, or `NA_character_`.

- `deps`: All package dependencies, in a data frame.

- `license`: Package license, might be `NA` for binary packages.

- `linkingto`: `LinkingTo` field from `DESCRIPTION`, or `NA_character_`.

- `enhances`: `Enhances` field from `DESCRIPTION`, or `NA_character_`.

- `os_type`: `unix` or `windows` for OS specific packages. Usually `NA`.

- `priority`: "optional", "recommended" or `NA`. (Base packages are
  normally not included in the list, so "base" should not appear here.)

- `md5sum`: MD5 sum, if available, may be `NA`.

- `sysreqs`: The `SystemRequirements` field, if available. This lists
  the required system libraries or other software for the package. This
  is usually available for CRAN and Bioconductor package and when it is
  explicitly available in the repository metadata.

- `published`: The time the package was published at, in GMT, `POSIXct`
  class.

The data frame contains some extra columns as well, these are for
internal use only.

## Examples

``` r
dir.create(cache_path <- tempfile())
cmc <- cranlike_metadata_cache$new(cache_path, bioc = FALSE)
cmc$list()
#> 
#> ✔ Updated metadata database: 5.51 MB in 4 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> # A data frame: 46,781 × 32
#>    package    version depends imports suggests needscompilation license
#>    <chr>      <chr>   <chr>   <chr>   <chr>    <chr>            <chr>  
#>  1 aae.pop    0.2.0   R (>= … stats,… knitr, … NA               Apache…
#>  2 AalenJoha… 1.0     NA      NA      knitr, … NA               GPL (>…
#>  3 aamatch    0.4.5   R (>= … iTOS, … DOS2, s… NA               GPL-2  
#>  4 AATtools   0.0.3   R (>= … magrit… NA       NA               GPL-3  
#>  5 ABACUS     1.0.0   R (>= … ggplot… rmarkdo… NA               GPL-3  
#>  6 abasequen… 0.1.0   NA      NA      NA       NA               GPL-3  
#>  7 abbreviate 0.1     NA      NA      testtha… NA               GPL-3  
#>  8 abc        2.2.2   R (>= … NA      NA       NA               GPL (>…
#>  9 abc.data   1.1     R (>= … NA      NA       NA               GPL (>…
#> 10 ABC.RAP    0.9.0   R (>= … graphi… knitr, … NA               GPL-3  
#> # ℹ 46,771 more rows
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>, …
cmc$list("pkgconfig")
#> # A data frame: 2 × 32
#>   package   version depends imports suggests   needscompilation license
#> * <chr>     <chr>   <chr>   <chr>   <chr>      <chr>            <chr>  
#> 1 pkgconfig 2.0.3   NA      utils   covr, tes… NA               MIT + …
#> 2 pkgconfig 2.0.3   NA      utils   covr, tes… no               MIT + …
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>
cmc$deps("pkgconfig")
#> # A data frame: 2 × 32
#>   package   version depends imports suggests   needscompilation license
#> * <chr>     <chr>   <chr>   <chr>   <chr>      <chr>            <chr>  
#> 1 pkgconfig 2.0.3   NA      utils   covr, tes… NA               MIT + …
#> 2 pkgconfig 2.0.3   NA      utils   covr, tes… no               MIT + …
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>
cmc$revdeps("pkgconfig", recursive = FALSE)
#> # A data frame: 18 × 32
#>    package   version depends  imports suggests needscompilation license
#>  * <chr>     <chr>   <chr>    <chr>   <chr>    <chr>            <chr>  
#>  1 agua      0.1.4   parsnip  "cli, … "covr, … NA               MIT + …
#>  2 biospear  1.0.2   R (>= 2… "cobs,…  NA      no               GPL-2  
#>  3 hms       1.1.4   NA       "cli, … "crayon… NA               MIT + …
#>  4 igraph    2.2.2   methods… "cli, … "ape (>… NA               GPL (>…
#>  5 jtools    2.3.1   R (>= 3… "cli, … "boot, … NA               GPL (>…
#>  6 pkgconfig 2.0.3   NA       "utils" "covr, … NA               MIT + …
#>  7 RSQLite   2.4.6   R (>= 3… "bit64… "callr,… NA               LGPL (…
#>  8 spbabel   0.6.0   R (>= 3… "dplyr… "testth… NA               GPL-3  
#>  9 tibble    3.3.1   R (>= 3… "cli, … "bench,… NA               MIT + …
#> 10 agua      0.1.4   parsnip  "cli, … "covr, … no               MIT + …
#> 11 biospear  1.0.2   R (>= 2… "cobs,…  NA      no               GPL-2  
#> 12 hms       1.1.4   NA       "cli, … "crayon… no               MIT + …
#> 13 igraph    2.2.2   methods… "cli, … "ape (>… yes              GPL (>…
#> 14 jtools    2.3.1   R (>= 3… "cli, … "boot, … no               GPL (>…
#> 15 pkgconfig 2.0.3   NA       "utils" "covr, … no               MIT + …
#> 16 RSQLite   2.4.6   R (>= 3… "bit64… "callr,… yes              LGPL (…
#> 17 spbabel   0.6.0   R (>= 3… "dplyr… "testth… no               GPL-3  
#> 18 tibble    3.3.1   R (>= 3… "cli, … "bench,… yes              MIT + …
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>
```
