# pkgcache and Posit Package Manager on Linux

## PPM and P3M

[Posit Package
Manager](https://posit.co/products/enterprise/package-manager/) (PPM) is
a Posit product to help organizations organize R and Python packages.

[Posit Public Package
Manager](https://packagemanager.posit.co/client/#/) (P3M) is a free
instance of PPM.

pkgcache deals with all PPM instances the same way currently, including
the P3M instance.

In the following, by PPM we mean any PPM instance.

## Why PPM?

- **Fast installation.** PPM has binary packages on Linux, for many
  distributions, for the last 5 R releases.
- **New packages on older R versions.** On Windows, new package versions
  are available as binary packages, on older R versions as well.
- **Time traveling.** Use an older state of the repository, as it was on
  a specific date.

## pkgcache and PPM on Linux

For a better user experience pkgcache handles PPM repositories specially
on Linux.

Call
[`pkgcache::ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md)
to decide if PPM builds binaries for your platform:

``` r
pkgcache::ppm_has_binaries()
```

    ## [1] TRUE

You can use
[`pkgcache::current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
to see if pkgcache detects your platform correctly:

``` r
pkgcache::current_r_platform()
```

    ## [1] "x86_64-pc-linux-gnu-ubuntu-24.04"

### Setting up PPM

Set the `repos` option to a PPM URL, as suggested by the PPM setup web
page. E.g. see
<https://packagemanager.posit.co/client/#/repos/2/overview> for P3M.

You can also set the `PKGCACHE_PPM_URL` environment variable to the base
URL of a PPM instance (e.g. <https://packagemanager.posit.co> for P3M),
and the `PKGCACHE_PPM_REPO` environment variable to the default PPM
repository. The
[`pkgcache::repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
and
[`pkgcache::repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
functions use these environment variables (if they are set) to construct
PPM URLs. See
[`?ppm_repo_url`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md)
for details.

### PPM repository URLs

A PPM repository URL for Linux binary packages has the following form:

    {base}/{repo}/__linux__/{binary_url}/{id}

where `{base}` is the base URL of the PPM instance, `{repo}` is a PPM
repository, `{binary_url}` is the code name of a release of a Linux
distribution (e.g. `rhel9`), and `{id}` is the identifier (or date) of a
PPM snapshot or `latest` for the latest snapshot.

[`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md)
lists platforms supported by a PPM instance:

``` r
pkgcache::ppm_platforms()
```

    ## # A data frame: 35 × 7
    ##    name        os    binary_url distribution release binaries platforms
    ##    <chr>       <chr> <chr>      <chr>        <chr>   <lgl>    <list>   
    ##  1 centos7     linux centos7    centos       7       TRUE     <chr [1]>
    ##  2 centos8     linux centos8    centos       8       TRUE     <chr [1]>
    ##  3 rhel9       linux rhel9      rockylinux   9       TRUE     <chr [7]>
    ##  4 rhel10      linux rhel10     rockylinux   10      TRUE     <chr [7]>
    ##  5 opensuse15  linux opensuse15 opensuse     15      TRUE     <chr [6]>
    ##  6 opensuse152 linux opensuse1… opensuse     15.2    TRUE     <chr [2]>
    ##  7 opensuse153 linux opensuse1… opensuse     15.3    TRUE     <chr [2]>
    ##  8 opensuse154 linux opensuse1… opensuse     15.4    TRUE     <chr [2]>
    ##  9 opensuse155 linux opensuse1… opensuse     15.5    TRUE     <chr [2]>
    ## 10 opensuse156 linux opensuse1… opensuse     15.6    TRUE     <chr [2]>
    ## # ℹ 25 more rows

The `binary_url` column contains the code name that you need to use in
the repository URL.

[`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)
lists all PPM snapshot dates and ids:

``` r
pkgcache::ppm_snapshots()
```

    ## # A data frame: 3,072 × 2
    ##    date       id        
    ##    <date>     <chr>     
    ##  1 2017-10-10 2017-10-10
    ##  2 2017-10-11 2017-10-11
    ##  3 2017-10-12 2017-10-12
    ##  4 2017-10-13 2017-10-13
    ##  5 2017-10-14 2017-10-14
    ##  6 2017-10-15 2017-10-15
    ##  7 2017-10-16 2017-10-16
    ##  8 2017-10-17 2017-10-17
    ##  9 2017-10-18 2017-10-18
    ## 10 2017-10-19 2017-10-19
    ## # ℹ 3,062 more rows

You can use the snapshot ids or dates in the PPM repository URL to
select a particular snapshot. E.g. these two repository URLs are
equivalent:

    https://packagemanager.posit.co/cran/__linux__/centos7/5
    https://packagemanager.posit.co/cran/__linux__/centos7/2017-10-10

### Linux platforms

pkgcache generalizes R’s “machine-vendor-OS” platform triplet on Linux
to include the distribution and release as well. E.g. the platform of
the machine building this document is:

``` r
pkgcache::current_r_platform()
```

    ## [1] "x86_64-pc-linux-gnu-ubuntu-24.04"

### PPM Linux binary packages

R does not have built-in support for binary Linux repositories, so PPM
offers binary Linux packages as source packages. This is possible
because R recognizes binary packages at installation time, and handles
them appropriately.

pkgcache detects binary PPM repositories, and constructs their platform
string:

``` r
print(
  pkgcache::meta_cache_list(package = c("ggplot2", "pkgcache"))[, c("package", "mirror", "platform")],
  width = Inf
)
```

    ## # A data frame: 4 × 3
    ##   package  mirror                                                     
    ## * <chr>    <chr>                                                      
    ## 1 ggplot2  https://packagemanager.posit.co/cran/__linux__/noble/latest
    ## 2 pkgcache https://packagemanager.posit.co/cran/__linux__/noble/latest
    ## 3 ggplot2  https://cran.rstudio.com                                   
    ## 4 pkgcache https://cran.rstudio.com                                   
    ##   platform                        
    ## * <chr>                           
    ## 1 x86_64-pc-linux-gnu-ubuntu-24.04
    ## 2 x86_64-pc-linux-gnu-ubuntu-24.04
    ## 3 source                          
    ## 4 source

#### Metadata discrepancies

Occasionally, the PPM metadata can be outdated, and pkgcache will
wrongly think that PPM has a binary for a package, whereas it does not,
or the opposite might also be true.

When pkgcache downloads a package as an update to the package cache, it
detects from the PPM response headers whether the downloaded package is
indeed of the expected type. If not, then pkgcache updates its knowledge
about the package.

### Supported R versions

Different PPM instances may support different R versions. The public P3M
instance supports the last five R releases. pkgcache will automatically
check if your configured PPM instance supports your R version, and
[`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
`repo_set()` will act accordingly. You can use the
[`pkgcache::ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md)
function to list the R versions supported by your instance:

``` r
pkgcache::ppm_r_versions()
```

    ## # A data frame: 6 × 1
    ##   r_version
    ##   <chr>    
    ## 1 4.5      
    ## 2 4.4      
    ## 3 4.3      
    ## 4 4.2      
    ## 5 4.1      
    ## 6 3.6

Note that these version numbers do not include the patch version. E.g.
`4.2` means that all `4.2.x` R versions are supported.

## pkgcache and PPM on Windows

A PPM instance can have support for Windows binaries, the public P3M
instance does support them.

The R package system supports Windows binaries out of the box, so
setting up PPM on Windows simply means setting the `repos` option to a
PPM repository. E.g. for the public P3M this would be something like:

``` r
options(repos = c(
  RSPM = "https://packagemanager.posit.co/cran/latest",
  CRAN = getOption("repos")[["CRAN"]]
))
```

Instead of `latest` you can use a different snapshot, of course, and you
can also use
[`repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
to construct the URL for your preferred snapshot.
