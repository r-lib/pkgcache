# List metadata of installed packages

This function is similar to
[`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).
See the differences below.

## Usage

``` r
parse_installed(
  library = .libPaths(),
  priority = NULL,
  lowercase = FALSE,
  reencode = TRUE,
  packages = NULL
)
```

## Arguments

- library:

  Character vector of library paths.

- priority:

  If not `NULL` then it may be a `"base"` `"recommended"` `NA` or a
  vector of these to select *base* packages, *recommended* packages or
  *other* packages. (These are the official, CRAN supported package
  priorities, but you may introduce others in non-CRAN packages.)

- lowercase:

  Whether to convert keys in `DESCRIPTION` to lowercase.

- reencode:

  Whether to re-encode strings in UTF-8, from the encodings specified in
  the `DESCRIPTION` files. Re-encoding is somewhat costly, and sometimes
  it is not important (e.g. when you only want to extract the
  dependencies of the installed packages).

- packages:

  If not `NULL`, then it must be a character vector, and only these
  packages will be listed.

## Details

Differences with
[`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html):

- `parse_installed()` cannot subset the extracted fields. (But you can
  subset the result.)

- `parse_installed()` does not cache the results.

- `parse_installed()` handles errors better. See Section 'Errors' below.
  \#' \* `parse_installed()` uses the `DESCRIPTION` files in the
  installed packages instead of the `Meta/package.rds` files. This
  should not matter, but because of a bug `Meta/package.rds` might
  contain the wrong `Archs` field on multi-arch platforms.

- `parse_installed()` reads *all* fields from the `DESCRIPTION` files.
  [`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html)
  only reads the specified fields.

- `parse_installed()` converts its output to UTF-8 encoding, from the
  encodings declared in the `DESCRIPTION` files.

- `parse_installed()` is considerably faster.

### Encodings

`parse_installed()` always returns its result in UTF-8 encoding. It uses
the `Encoding` fields in the `DESCRIPTION` files to learn their
encodings. `parse_installed()` does not check that an UTF-8 file has a
valid encoding. If it fails to convert a string to UTF-8 from another
declared encoding, then it leaves it as `"bytes"` encoded, without a
warning.

### Errors

pkgcache silently ignores files and directories inside the library
directory.

The result also omits broken package installations. These include

- packages with invalid `DESCRIPTION` files, and

- packages the current user have no access to.

These errors are reported via a condition with class
`pkgcache_broken_install`. The condition has an `errors` entry, which is
a data frame with columns

- `file`: path to the `DESCRIPTION` file of the broken package,

- `error`: error message for this particular failure.

If you intend to handle broken package installation, you need to catch
this condition with
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).
