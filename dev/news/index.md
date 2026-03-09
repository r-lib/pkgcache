# Changelog

## pkgcache (development version)

- pkgcache now supports comments in `DESCRIPTION` and `PACKAGES` files.
  Current R-devel supports this since
  <https://github.com/wch/r-source/commit/92d9660517ceae66d422a510dc58e0840d55cdfc>.
  Comments are lines that start with a hash (`#`), without leading
  whitespace. Comments within values are also supported
  ([\#130](https://github.com/r-lib/pkgcache/issues/130)).

## pkgcache 2.2.4

CRAN release: 2025-05-26

- [`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
  now parses files ending with an extra newline correctly
  ([\#122](https://github.com/r-lib/pkgcache/issues/122)).

- pkgcache now supports authenticated repositories, see
  [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md)
  for details.

- [`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md)
  now works correctly on all platforms
  ([\#121](https://github.com/r-lib/pkgcache/issues/121)).

- The output of
  [`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md)
  now has a new column called `platform`, which is a list column of
  character vectors, listing all possible matching platforms.

- pkgcache now detects the platform and R version of R Universe
  binaries.

## pkgcache 2.2.3

CRAN release: 2024-09-12

- The metadata cache now does not use source URLs for packages in
  `Archive` on Posit Package Manager repositories. This URLs may serve a
  different package, even a source package when the main URL for the
  same package serves a binary package. The alternative URLs are not
  needed on PPM, anyway, because PPM is in a consistent state w.r.t.
  metadata and package files
  (<https://github.com/r-lib/pak/issues/623>).

- [`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
  now does not throw a warning for empty `PACKAGES*` files
  ([\#107](https://github.com/r-lib/pkgcache/issues/107)).

- `repo_set()` and the `ppm_*()` functions,
  e.g. [`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md),
  now work again after the PPM API changes
  ([\#110](https://github.com/r-lib/pkgcache/issues/110),
  [\#115](https://github.com/r-lib/pkgcache/issues/115)).

## pkgcache 2.2.2

CRAN release: 2024-04-04

- pkgcache now treats R 4.5.x (current R-devel) macOS binaries
  correctly. It is also more robust to changes the in macOS platform.

## pkgcache 2.2.1

CRAN release: 2023-12-10

- pkgcache now does a better job when matching the R version to a
  Bioconductor version.

## pkgcache 2.2.0

CRAN release: 2023-07-18

- New `pkg.current_platform` option and `PKG_CURRENT_PLATFORM`
  environment variable to override the detected platform.

- In
  [`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  and
  [`repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  the `MRAN@` prefix is now deprecated and resolves to PPM, because MRAN
  will be retired soon. See more at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

- The metadata cache now has `SystemRequirements` information for
  Bioconductor packages.

## pkgcache 2.1.1

CRAN release: 2023-06-14

- [`parse_installed()`](https://r-lib.github.io/pkgcache/dev/reference/parse_installed.md)
  now has a `packages` argument, to list only a subset of all packages.

- [`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
  can now parse `PACKAGES` files with trailing whitespace
  ([\#93](https://github.com/r-lib/pkgcache/issues/93)).

- The Bioconductor repositories now include the ‘books’ repository,
  available since Bioconductor 3.12.

## pkgcache 2.1.0

CRAN release: 2023-04-18

- pkgcache now supports binary packages on x86_64 macOS and R 4.3.0 and
  later ([\#89](https://github.com/r-lib/pkgcache/issues/89)).

- Better Posit Package Manager (PPM) support. New
  [`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md),
  [`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md),
  [`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md),
  [`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)
  and
  [`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md)
  functions to help interacting with PPM. See the new ‘pkgcache and
  Posit Package Manager on Linux’ article at
  <https://r-lib.github.io/pkgcache>.
  ([\#47](https://github.com/r-lib/pkgcache/issues/47) and
  r-lib/pkgdepends#186).

## pkgcache 2.0.4

CRAN release: 2022-12-16

- Update R version -\> Bioconductor version mapping. R 4.2.x now maps to
  Bioconductor 3.16.

## pkgcache 2.0.3

CRAN release: 2022-10-26

- The `built` and `sysreqs` columns of the metadata case are always
  character vectors now, and not logicals, as it used to be in some
  edges cases in the past.

- The `deps` column of the metadata cache is not a tibble any more, but
  a data frame with a `tbl` class, as it should be.

- `cran_archive_*()` functions now only download the metadata if it is
  newer than what you have currently.

- [`cran_archive_cleanup()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md)
  now does not ignore the `force` argument.

- The `sources` column in the metadata cache now has the correct URL for
  packages in the CRAN archive
  (<https://github.com/r-lib/pak/issues/425>).

## pkgcache 2.0.2

CRAN release: 2022-09-08

- pkgcache error messages are better now.

- pkgcache now does not compress the metadata cache files, which makes
  loading the metadata cache faster.

## pkgcache 2.0.1

CRAN release: 2022-02-18

No user visible changes.

## pkgcache 2.0.0

CRAN release: 2022-02-16

### BREAKING CHANGE

- Starting from version 2.0.0 pkgcache returns data frames instead of
  tibbles. While data frames and tibbles are very similar, they are not
  completely compatible. To convert the output of pkgcache to tibbles
  call the `as_tibble()` function on them.

  pkgcache loads the pillar package at startup, if available, and uses
  it to improve the printing of pkgcache data frames.

### OTHER CHANGES

- [`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
  now automatically determines the type of the `PACKAGES*` file, instead
  of relying on file extensions.

- pkgcache can now call back again to a `PACKAGES` file if `PACKAGES.gz`
  is not available. (This fixes a regression in pkgcache 1.3.0.)

- pkgcache now uses HTTP 1.1 on Linux as well, because of crashes with
  HTTP/2.

- pkgcache now supports `file:///`, repositories, i.e. repositories on
  the local file system.

## pkgcache 1.3.0

CRAN release: 2021-11-29

- pkgcache now works better on M1 macs.

- [`current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  does a much better job now. In particular, on Linux it includes the
  name and release of the distribution. The new
  [`current_r_platform_data()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  function returns the platform information as a data frame, instead of
  a single string.

- Metadata is now more accurate for Windows packages that are typically
  not multi-arch any more on R 4.2.0 (current R-devel).

- pkgcache has its own DCF metadata parser now, which is much faster,
  and it parses all fields of `PACAKGES*` and `DESCRIPTION` files.

- New
  [`parse_installed()`](https://r-lib.github.io/pkgcache/dev/reference/parse_installed.md)
  function to get the metadata of all installed packages in a library.
  It uses the new DCF parser, so it is quite fast.

- [`meta_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  and related functions now correctly set the `rversion` column of
  source R packages to `"*"`.

- pkgcache now uses HTTP 1.1 on macOS, to work around a possible
  slowdown issue with libcurl for HTTP/2.

- pkgcache now uses our extra metadata (file sizes, system requirements,
  etc.) for RStudio Package Manager (RSPM) repositories as well, as long
  as they are named `RSPM` in `getOption("repos")`.

## pkgcache 1.2.2

CRAN release: 2021-05-19

- The default location of the cache has changed to align with the
  standard [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)
  cache location. To clean up your old cache call
  `pkgcache:::cleanup_old_cache_dir()`.

## pkgcache 1.2.1

CRAN release: 2021-04-14

No user visible changes.

## pkgcache 1.2.0

CRAN release: 2021-03-01

- New
  [`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md),
  [`repo_get()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md),
  [`repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  and
  [`with_repo()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  functions to query and manipulate repositories.

- `meta_cache_*()` functions now handle `getOption("repos")` changes
  correctly.

- Failed metadata downloads now do not trigger metadata updates
  ([\#52](https://github.com/r-lib/pkgcache/issues/52)).

- New
  [`bioc_release_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md),
  [`bioc_devel_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md),
  [`bioc_repos()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  helper functions to deal with Bioconductor repositories.

- Metadata cache functions,
  e.g. [`meta_cache_deps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  etc. now allow specifying the dependency types in all lowercase
  ([\#54](https://github.com/r-lib/pkgcache/issues/54)).

## pkgcache 1.1.1

CRAN release: 2020-10-11

- `package_cache` now does not fail if the web server does not send an
  `Etag` header when downloading packages.

- `package_cache` has now much relaxed HTTP timeouts, and handles
  downloading many packages (slowly) much better.

- The package download progress bar can now be suppressed by setting the
  `pkg.show_progress` option to `FALSE`.

## pkgcache 1.1.0

CRAN release: 2020-07-07

- New
  [`repo_status()`](https://r-lib.github.io/pkgcache/dev/reference/repo_status.md)
  function to query the status and response time of CRAN-like
  repositories.

- New
  [`bioc_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  and
  [`bioc_version_map()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  functions to query Bioconductor repositories.

- pkgcache now does not fail if some repositories do not provide some
  package types.

- New
  [`current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md),
  [`default_cran_mirror()`](https://r-lib.github.io/pkgcache/dev/reference/default_cran_mirror.md)
  and
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  functions.

- pkgcache now works for R 4.0.x macOS binaries.

## pkgcache 1.0.7

CRAN release: 2020-04-18

- Metadata is now cached in RDS version 2 formats, so metadata written
  by newer R version can be used by older R versions as well
  ([\#36](https://github.com/r-lib/pkgcache/issues/36)).

## pkgcache 1.0.6

CRAN release: 2020-01-30

- HTTP timeouts are now much better, and by default they are defined in
  terms of download speed, instead of total download time
  ([\#29](https://github.com/r-lib/pkgcache/issues/29)).

- pkgcache now tries to download metadata from the `PACKAGES` file, if
  it cannot find `PACKAGES.gz` ([@timmsm](https://github.com/timmsm),
  [\#27](https://github.com/r-lib/pkgcache/issues/27)).

- pkgcache is now less verbose when updating or loading metadata.

## pkgcache 1.0.5

CRAN release: 2019-05-31

- Fix a bug in the download functions, that broke pak downloads.

## pkgcache 1.0.4

CRAN release: 2019-05-13

- Fix handling of Bioconductor versions and repositories, see README for
  the details.

- Now different versions of pkgcache, that potentially have different
  metadata format, can share the same metadata cache directory.

## pkgcache 1.0.3

CRAN release: 2019-01-11

- Fix concurrency issues when the async API is used multiple times in
  the same event loop.

- Make package compatible with tibble \>= 2.0.0.

- Add
  [`meta_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  and a [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `cranlike_metadata_cache`. Return information about a metadata cache
  instance.

## pkgcache 1.0.2

CRAN release: 2018-12-31

- First public release
