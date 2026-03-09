# List all available Posit Package Manager (PPM) snapshots

List all available Posit Package Manager (PPM) snapshots

## Usage

``` r
ppm_snapshots()
```

## Value

Data frame with two columns:

- `date`: the time the snapshot was taken, a `POSIXct` vector,

- `id`: integer id of the snapshot, this can be used in the repository
  URL.

## Details

The repository URL of a snapshot has the following form on Windows:

    {base}/{repo}/{id}

where `{base}` is the base URL for PPM (see
[`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md))
and `{id}` is either the date or id of the snapshot, or `latest` for the
latest snapshot. E.g. these are equivalent:

    https://packagemanager.posit.co/cran/5
    https://packagemanager.posit.co/cran/2017-10-10

On a Linux distribution that has PPM support, the repository URL that
contains the binary packages looks like this:

    {base}/{repo}/__linux__/{binary_url}/{id}

where `{id}` is as before, and `{binary_url}` is a code name for a
release of a supported Linux distribution. See the `binary_url` column
of the result of
[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md)
for these code names.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/dev/>.

Other PPM functions:
[`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md),
[`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md)

## Examples

``` r
ppm_snapshots()
#> # A data frame: 3,072 × 2
#>    date       id        
#>    <date>     <chr>     
#>  1 2017-10-10 2017-10-10
#>  2 2017-10-11 2017-10-11
#>  3 2017-10-12 2017-10-12
#>  4 2017-10-13 2017-10-13
#>  5 2017-10-14 2017-10-14
#>  6 2017-10-15 2017-10-15
#>  7 2017-10-16 2017-10-16
#>  8 2017-10-17 2017-10-17
#>  9 2017-10-18 2017-10-18
#> 10 2017-10-19 2017-10-19
#> # ℹ 3,062 more rows
```
