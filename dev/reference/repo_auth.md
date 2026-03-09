# Authenticated repositories

`repo_auth()` lists authentication information for all configured
repositories.

## Usage

``` r
repo_auth(
  r_version = getRversion(),
  bioc = TRUE,
  cran_mirror = default_cran_mirror(),
  check_credentials = TRUE
)
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

- check_credentials:

  Whether to check that credentials are available for the authenticated
  repositories.

## Value

Data frame with columns:

- all columns from the output of
  [`repo_get()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md),

- `auth_domains`: authentication domains. pkgcache tries to find the
  credentials for these domains, until the search is successful or all
  domains fail. This column is a list column of character vectors.

- `auth_domain`: if the credential lookup is successful, then this is
  the authentication domain that was used to get the credentials.

- `auth_source`: where the credentials were found. E.g. `keyring:macos`
  means it was in the default macos keyring.

- `auth_error`: for failed credential searches this is the description
  of why the search failed. E.g. maybe the keyring package is not
  installed, or pkgcache found no credentials for any of the
  authentication domains.

## Details

pkgcache supports HTTP basic authentication when interacting with
CRAN-like repositories. To use authentication, include a username in the
repo URL:

    https://<username>@<repo-host>/<repo-path>

pkgcache tries to obtain the passwords for the authenticated CRAN-like
repos from two sources, in this order:

1.  A "netrc" file. If the `NETRC` environment variable is set, it
    should point to a file in "netrc" format. Otherwise pkgcache uses
    the `~/.netrc` file in the current user's home directory, if it
    exists. On Windows it also tries the `~/_netrc` file.

2.  The system's credential store, via the keyring package.

See the [documentation of
libcurl](https://curl.se/libcurl/c/CURLOPT_NETRC.html) for details about
the format of the netrc file.

When looking for the password in the system credential store, pkgcache
looks at the following keys, in this order:

    https://<username>@repo-host/<repo-path>
    https://repo-host/<repo-path>
    https://<username>@repo-host
    https://repo-host

To add an authenticated repository use
[`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
with the `username` argument. Alternatively, you can set the `repos`
option directly using
[`base::options()`](https://rdrr.io/r/base/options.html) and including
the username in the repository URL.
