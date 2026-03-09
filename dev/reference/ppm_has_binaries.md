# Does PPM build binary packages for the current platform?

Does PPM build binary packages for the current platform?

## Usage

``` r
ppm_has_binaries()
```

## Value

`TRUE` or `FALSE`.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/dev/>.

Other PPM functions:
[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md),
[`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)

## Examples

``` r
current_r_platform()
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
ppm_has_binaries()
#> [1] TRUE
```
