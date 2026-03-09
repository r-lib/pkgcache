# Returns the current Posit Package Manager (PPM) repository URL

Returns the current Posit Package Manager (PPM) repository URL

## Usage

``` r
ppm_repo_url()
```

## Value

String scalar, the repository URL of the configured PPM instance. If no
PPM instance is configured, then the URL of the Posit Public Package
Manager instance. It includes the repository name, e.g.
`https://packagemanager.posit.co/all`.

## Details

This URL has the form `{base}/{repo}`, e.g.
`https://packagemanager.posit.co/all`.

To configure a hosted PPM instance, set the `PKGCACHE_PPM_URL`
environment variable to the base URL (e.g.
`https://packagemanager.posit.co`).

To use
[`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
with PPM snapshots, you may also set the `PKGCACHE_PPM_REPO` environment
variable to the name of the default repository.

On Linux, instead of setting these environment variables, you can also
add a PPM repository to the `repos` option, see
[`base::options()`](https://rdrr.io/r/base/options.html). If the
environment variables are not set, then `ppm_repo_url()` will try to
extract the PPM base URL and repository name from this option.

If the `PKGCACHE_PPM_URL` environment variable is not set, and the
`repos` option does not contain a PPM URL (on Linux), then pkgcache uses
the public PPM instance at `https://packagemanager.posit.co`, with the
`cran` repository.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/dev/>.

[`repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
and
[`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
to find and configure PPM snapshots.

Other PPM functions:
[`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md),
[`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md),
[`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)

## Examples

``` r
ppm_repo_url()
#> [1] "https://packagemanager.posit.co/cran"
```
