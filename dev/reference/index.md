# Package index

## CRAN-like metadata cache

- [`cranlike_metadata_cache`](https://r-lib.github.io/pkgcache/dev/reference/cranlike_metadata_cache.md)
  : Metadata cache for a CRAN-like repository
- [`cran_archive_cache`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_cache.md)
  : Cache for CRAN archive data
- [`cran_archive_list()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md)
  [`cran_archive_update()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md)
  [`cran_archive_cleanup()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md)
  [`cran_archive_summary()`](https://r-lib.github.io/pkgcache/dev/reference/cran_archive_list.md)
  : Data about older versions of CRAN packages
- [`get_cranlike_metadata_cache()`](https://r-lib.github.io/pkgcache/dev/reference/get_cranlike_metadata_cache.md)
  : The R6 object that implements the global metadata cache
- [`meta_cache_deps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  [`meta_cache_revdeps()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  [`meta_cache_update()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  [`meta_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  [`meta_cache_cleanup()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  [`meta_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/meta_cache_deps.md)
  : Query CRAN(like) package data

## Package cache

- [`package_cache`](https://r-lib.github.io/pkgcache/dev/reference/package_cache.md)
  : A simple package cache
- [`pkg_cache_summary()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  [`pkg_cache_list()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  [`pkg_cache_find()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  [`pkg_cache_get_file()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  [`pkg_cache_delete_files()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  [`pkg_cache_add_file()`](https://r-lib.github.io/pkgcache/dev/reference/pkg_cache_api.md)
  : Functions to query and manipulate the package cache

## Repositories

- [`ppm_has_binaries()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_has_binaries.md)
  : Does PPM build binary packages for the current platform?
- [`ppm_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_platforms.md)
  : List all platforms supported by Posit Package Manager (PPM)
- [`ppm_r_versions()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_r_versions.md)
  : List all R versions supported by Posit Package Manager (PPM)
- [`ppm_repo_url()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_repo_url.md)
  : Returns the current Posit Package Manager (PPM) repository URL
- [`ppm_snapshots()`](https://r-lib.github.io/pkgcache/dev/reference/ppm_snapshots.md)
  : List all available Posit Package Manager (PPM) snapshots
- [`repo_auth()`](https://r-lib.github.io/pkgcache/dev/reference/repo_auth.md)
  : Authenticated repositories
- [`repo_get()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  [`repo_resolve()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  [`repo_add()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  [`with_repo()`](https://r-lib.github.io/pkgcache/dev/reference/repo_get.md)
  : Query and set the list of CRAN-like repositories
- [`repo_status()`](https://r-lib.github.io/pkgcache/dev/reference/repo_status.md)
  : Show the status of CRAN-like repositories

## Parsing

- [`parse_installed()`](https://r-lib.github.io/pkgcache/dev/reference/parse_installed.md)
  : List metadata of installed packages

- [`parse_packages()`](https://r-lib.github.io/pkgcache/dev/reference/parse_packages.md)
  :

  Parse a repository metadata `PACAKGES*` file

## Configuration

- [`bioc_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  [`bioc_version_map()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  [`bioc_devel_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  [`bioc_release_version()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  [`bioc_repos()`](https://r-lib.github.io/pkgcache/dev/reference/bioc_version.md)
  : Query Bioconductor version information
- [`current_r_platform()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  [`current_r_platform_data()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  [`default_platforms()`](https://r-lib.github.io/pkgcache/dev/reference/current_r_platform.md)
  : R platforms
- [`default_cran_mirror()`](https://r-lib.github.io/pkgcache/dev/reference/default_cran_mirror.md)
  : Query the default CRAN repository for this session

## Utilities

- [`get_graphics_api_version()`](https://r-lib.github.io/pkgcache/dev/reference/get_graphics_api_version.md)
  : Query the version of the graphics API
- [`get_internals_id()`](https://r-lib.github.io/pkgcache/dev/reference/get_internals_id.md)
  : Query UUID identifying the version of the R API
