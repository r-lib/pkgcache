# List all R versions supported by Posit Package Manager (PPM)

List all R versions supported by Posit Package Manager (PPM)

## Usage

``` r
ppm_r_versions()
```

## Value

Data frame with columns:

- `r_version`: minor R versions, i.e. version numbers containing the
  first two components of R versions supported by this PPM instance.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/dev/>.

Other PPM functions:
[`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md),
[`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)

## Examples

``` r
ppm_r_versions()
#> # A data frame: 6 × 1
#>   r_version
#>   <chr>    
#> 1 4.5      
#> 2 4.4      
#> 3 4.3      
#> 4 4.2      
#> 5 4.1      
#> 6 3.6      
```
