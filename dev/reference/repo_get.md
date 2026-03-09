# Query and set the list of CRAN-like repositories

pkgcache uses the `repos` option, see
[`options()`](https://rdrr.io/r/base/options.html). It also
automatically uses the current Bioconductor repositories, see
[`bioc_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md).
These functions help to query and manipulate the `repos` option.

## Usage

``` r
repo_get(
  r_version = getRversion(),
  bioc = TRUE,
  cran_mirror = default_cran_mirror()
)

repo_resolve(spec, username = NULL)

repo_add(..., .list = NULL, username = NULL)

with_repo(repos, expr)
```

## Arguments

- r_version:

  R version(s) to use for the Bioconductor repositories, if `bioc` is
  `TRUE`.

- bioc:

  Whether to add Bioconductor repositories, even if they are not
  configured in the `repos` option.

- cran_mirror:

  The CRAN mirror to use, see
  [`default_cran_mirror()`](https://r-lib.github.io/pkgcache/dev/reference/default_cran_mirror.md).

- spec:

  A single repository specification, a possibly named character scalar.
  See details below.

- username:

  User name to set, for authenticated repositories, see
  [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md).

- ...:

  Repository specifications. See details below.

- .list:

  List or character vector of repository specifications, see details
  below.

- repos:

  A list or character vector of repository specifications.

- expr:

  R expression to evaluate.

## Value

`repo_get()` returns a data frame with columns:

- `name`: repository name. Names are informational only.

- `url`: repository URL.

- `type`: repository type. This is also informational, currently it can
  be `cran` for CRAN, `bioc` for a Bioconductor repository, and
  `cranlike`: for other repositories.

- `r_version`: R version that is supposed to be used with this
  repository. This is only set for Bioconductor repositories. It is `*`
  for others. This is also informational, and not used when retrieving
  the package metadata.

- `bioc_version`: Bioconductor version. Only set for Bioconductor
  repositories, and it is `NA` for others.

- `username`: user name, for authenticated repositories.

- `has_password`: whether `repo_get()` could find the password for this
  repository. Call
  [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md)
  for more information if the credential lookup failed.

`repo_resolve()` returns a named character vector, with the URL(s) of
the repository.

`repo_add()` returns the same data frame as `repo_get()`, invisibly.

`with_repo()` returns the value of `expr`.

## Details

`repo_get()` queries the repositories pkgcache uses. It uses the `repos`
option (see [options](https://rdrr.io/r/base/options.html)), and also
the default Bioconductor repository.

`repo_resolve()` resolves a single repository specification to a
repository URL.

`repo_add()` adds a new repository to the `repos` option. (To remove a
repository, call `option()` directly, with the subset that you want to
keep.)

`with_repo()` temporarily adds the repositories in `repos`, evaluates
`expr`, and then resets the configured repositories.

## Repository specifications

The format of a repository specification is a named or unnamed character
scalar. If the name is missing, pkgcache adds a name automatically. The
repository named `CRAN` is the main CRAN repository, but otherwise names
are informational.

Currently supported repository specifications:

- URL pointing to the root of the CRAN-like repository. Example:

      https://cloud.r-project.org

- `PPM@latest`, PPM (Posit Package Manager, formerly RStudio Package
  Manager), the latest snapshot.

- `PPM@<date>`, PPM (Posit Package Manager, formerly RStudio Package
  Manager) snapshot, at the specified date.

- `PPM@<package>-<version>` PPM snapshot, for the day after the release
  of `<version>` of `<package>`.

- `PPM@R-<version>` PPM snapshot, for the day after R `<version>` was
  released.

Still works for dates starting from 2017-10-10, but now deprecated,
because MRAN is discontinued:

- `MRAN@<date>`, MRAN (Microsoft R Application Network) snapshot, at the
  specified date.

- `MRAN@<package>-<version>` MRAN snapshot, for the day after the
  release of `<version>` of `<package>`.

- `MRAN@R-<version>` MRAN snapshot, for the day after R `<version>` was
  released.

Notes:

- See more about PPM at <https://packagemanager.posit.co/client/#/>.

- The `RSPM@` prefix is still supported and treated the same way as
  `PPM@`.

- The MRAN service is now retired, see
  `https://techcommunity.microsoft.com/blog/azuresqlblog/microsoft-r-application-network-retirement/3707161`
  for details.

- `MRAN@...` repository specifications now resolve to PPM, but note that
  PPM snapshots are only available from 2017-10-10. See more about this
  at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

- All dates (or times) can be specified in the ISO 8601 format.

- If PPM does not have a snapshot available for a date, the next
  available date is used.

- Dates that are before the first, or after the last PPM snapshot will
  trigger an error.

- Unknown R or package versions will trigger an error.

## See also

Other repository functions:
[`repo_status()`](https://r-lib.github.io/pkgcache/dev/reference/repo_status.md)

## Examples

``` r
repo_get()
#> # A data frame: 7 × 5
#>   name          url                        type  r_version bioc_version
#> * <chr>         <chr>                      <chr> <chr>     <chr>       
#> 1 RSPM          https://packagemanager.po… cran… *         NA          
#> 2 CRAN          https://cran.rstudio.com   cran  *         NA          
#> 3 BioCsoft      https://bioconductor.org/… bioc  4.5.2     3.22        
#> 4 BioCann       https://bioconductor.org/… bioc  4.5.2     3.22        
#> 5 BioCexp       https://bioconductor.org/… bioc  4.5.2     3.22        
#> 6 BioCworkflows https://bioconductor.org/… bioc  4.5.2     3.22        
#> 7 BioCbooks     https://bioconductor.org/… bioc  4.5.2     3.22        
repo_resolve("PPM@2021-01-21")
#>                                                              CRAN 
#> "https://packagemanager.posit.co/cran/__linux__/noble/2021-01-21" 
#' repo_resolve("PPM@dplyr-1.0.0")
#' repo_resolve("PPM@R-4.0.0")
with_repo(c(CRAN = "PPM@dplyr-1.0.0"), repo_get())
#> # A data frame: 7 × 5
#>   name          url                        type  r_version bioc_version
#> * <chr>         <chr>                      <chr> <chr>     <chr>       
#> 1 RSPM          https://packagemanager.po… cran… *         NA          
#> 2 CRAN          https://packagemanager.po… cran  *         NA          
#> 3 BioCsoft      https://bioconductor.org/… bioc  4.5.2     3.22        
#> 4 BioCann       https://bioconductor.org/… bioc  4.5.2     3.22        
#> 5 BioCexp       https://bioconductor.org/… bioc  4.5.2     3.22        
#> 6 BioCworkflows https://bioconductor.org/… bioc  4.5.2     3.22        
#> 7 BioCbooks     https://bioconductor.org/… bioc  4.5.2     3.22        
with_repo(c(CRAN = "PPM@dplyr-1.0.0"), meta_cache_list(package = "dplyr"))
#> 
#> ✔ Updated metadata database: 4.73 MB in 5 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> # A data frame: 2 × 32
#>   package version depends     imports suggests needscompilation license
#> * <chr>   <chr>   <chr>       <chr>   <chr>    <chr>            <chr>  
#> 1 dplyr   1.2.0   R (>= 4.1.… "cli (… "broom,… NA               MIT + …
#> 2 dplyr   0.8.5   R (>= 3.2.… "ellip… "bit64,… yes              MIT + …
#> # ℹ 25 more variables: linkingto <chr>, enhances <chr>,
#> #   license_restricts_use <chr>, os_type <chr>, path <chr>,
#> #   priority <chr>, license_is_foss <chr>, archs <chr>, repodir <chr>,
#> #   rversion <chr>, platform <chr>, ref <chr>, type <chr>,
#> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
#> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>

with_repo(c(CRAN = "MRAN@2018-06-30"), summary(repo_status()))
#> Repository summary:                       source          
#> RSPM          @ packagemanager.posit.co     ✔      (308ms)
#> CRAN          @ packagemanager.posit.co     ✔      (352ms)
#> BioCsoft      @ bioconductor.org            ✔      (80ms )
#> BioCann       @ bioconductor.org            ✔      (307ms)
#> BioCexp       @ bioconductor.org            ✔      (320ms)
#> BioCworkflows @ bioconductor.org            ✔      (350ms)
#> BioCbooks     @ bioconductor.org            ✔      (322ms)
```
