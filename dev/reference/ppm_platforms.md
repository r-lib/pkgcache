# List all platforms supported by Posit Package Manager (PPM)

List all platforms supported by Posit Package Manager (PPM)

## Usage

``` r
ppm_platforms()
```

## Value

Data frame with columns:

- `name`: platform name, this is essentially an identifier,

- `os`: operating system, `linux`, `windows` or `macOS` currently,

- `binary_url`: the URL segment of the binary repository URL of this
  platform, see
  [`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md).

- `distribution`: for Linux platforms the name of the distribution,

- `release`: for Linux platforms, the name of the release,

- `binaries`: whether PPM builds binaries for this platform.

- `platforms`: a list column of character vectors; for each row they
  list all possible matching distribution and release strings. Each
  string can be a fixes strings, but if it starts and ends with a
  forward string, then it is used as regular expression.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/dev/>.

Other PPM functions:
[`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
[`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)

## Examples

``` r
ppm_platforms()
#> # A data frame: 35 × 7
#>    name        os    binary_url distribution release binaries platforms
#>    <chr>       <chr> <chr>      <chr>        <chr>   <lgl>    <list>   
#>  1 centos7     linux centos7    centos       7       TRUE     <chr [1]>
#>  2 centos8     linux centos8    centos       8       TRUE     <chr [1]>
#>  3 rhel9       linux rhel9      rockylinux   9       TRUE     <chr [7]>
#>  4 rhel10      linux rhel10     rockylinux   10      TRUE     <chr [7]>
#>  5 opensuse15  linux opensuse15 opensuse     15      TRUE     <chr [6]>
#>  6 opensuse152 linux opensuse1… opensuse     15.2    TRUE     <chr [2]>
#>  7 opensuse153 linux opensuse1… opensuse     15.3    TRUE     <chr [2]>
#>  8 opensuse154 linux opensuse1… opensuse     15.4    TRUE     <chr [2]>
#>  9 opensuse155 linux opensuse1… opensuse     15.5    TRUE     <chr [2]>
#> 10 opensuse156 linux opensuse1… opensuse     15.6    TRUE     <chr [2]>
#> # ℹ 25 more rows
```
