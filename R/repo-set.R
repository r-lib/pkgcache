
#' Query and set the list of CRAN-like repositories
#'
#' pkgcache uses the `repos` option, see [options()]. It also automatically
#' uses the current Bioconductor repositories, see [bioc_version()].
#' These functions help to query and manipulate the `repos` option.
#'
#' @details
#' `repo_get()` queries the repositories pkgcache uses. It uses the
#' `repos` option (see [options]), and also the default Bioconductor
#' repository.
#'
#' @param bioc Whether to add Bioconductor repositories, even if they
#' are not configured in the `repos` option.
#' @param r_version R version(s) to use for the Bioconductor repositories,
#' if `bioc` is `TRUE`.
#' @param cran_mirror The CRAN mirror to use, see
#'   [default_cran_mirror()].
#' 
#' @return
#' `repo_get()` returns a data frame with columns:
#' * `name`: repository name. Names are informational only.
#' * `url`: repository URL.
#' * `type`: repository type. This is also informational, currently it
#'   can be `cran` for CRAN, `bioc` for a Bioconductor repository, and
#'   `cranlike`: for other repositories.
#' * `r_version`: R version that is supposed to be used with this
#'   repository. This is only set for Bioconductor repositories. It is `*`
#'   for others. This is also informational, and not used when retrieving
#'   the package metadata.
#' * `bioc_version`: Bioconductor version. Only set for Biocondictor
#'   repositories, and it is `NA` for others.
#'
#' @export
#' @family repository functions
#'
#' @examples
#' repo_get()

repo_get <- function(r_version = getRversion(), bioc = TRUE,
                     cran_mirror = default_cran_mirror()) {
  cmc__get_repos(
    getOption("repos"),
    bioc = bioc,
    cran_mirror = cran_mirror,
    as.character(r_version)
  )
}

#' @rdname repo_get
#' @param spec A single repository specification, a possibly named
#'   character scalar. See details below.
#' @details
#'   `repo_resolve()` resolves a single repository specification to a
#'   repository URL.
#' @return
#'   `repo_resolve()` returns a named character vector, with the URL(s) of
#'   the repository.
#'
#' @export
#' @examplesIf pkgcache:::is_online()
#' repo_resolve("MRAN@2020-01-21")
#' repo_resolve("RSPM@2020-01-21")
#' repo_resolve("MRAN@dplyr-1.0.0")
#' repo_resolve("RSPM@dplyr-1.0.0")
#' repo_resolve("MRAN@R-4.0.0")
#' repo_resolve("RSPM@R-4.0.0")

repo_resolve <- function(spec) {
  repo_sugar(spec, names(spec))
}

#' @rdname repo_get
#' @param ...  Repository specifications. See details below.
#' @param .list List or character vector of repository specifications,
#'   see details below.
#' @details
#'   `repo_add()` adds a new repository to the `repos` option. (To remove
#'   a repository, call `option()` directly, with the subset that you want
#'   to keep.)
#' @return
#'   `repo_add()` returns the same data frame as `repo_get()`, invisibly.
#' @export

repo_add <- function(..., .list = NULL) {
  repo_add_internal(..., .list = .list)
  invisible(repo_get())
}

repo_add_internal <- function(..., .list = NULL) {
  new <- c(list(...), .list)

  if (length(new) == 0) return(invisible(repo_get()))

  toadd <- unlist(mapply(
    repo_sugar,
    new,
    names(new) %||% rep("", length(new)),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))

  names(toadd) <- make.unique(names(toadd), sep = "_")

  current <- getOption("repos")
  upd <- modify_vec(current, toadd)

  options(repos = upd)
}

#' @rdname repo_get
#' @param repos A list or character vector of repository specifications.
#' @param expr R expression to evaluate.
#' @details
#'   `with_repo()` temporarily adds the repositories in `repos`,
#'   evaluates `expr`, and then resets the configured repositories.
#' @return
#'   `with_repo()` returns the value of `expr`.
#' @export
#' @examplesIf pkgcache:::is_online()
#' with_repo(c(CRAN = "RSPM@dplyr-1.0.0"), repo_get())
#' with_repo(c(CRAN = "RSPM@dplyr-1.0.0"), meta_cache_list(package = "dplyr"))
#'
#' with_repo(c(CRAN = "MRAN@2018-06-30"), summary(repo_status()))

with_repo <- function(repos, expr) {
  old <- repo_add_internal(.list = repos)
  on.exit(options(old))
  expr
}

# ## RSPM
#
# RSPM@2021-02-04T14:25:00Z
# RSPM@2021-02-04
# RSPM@dplyr@1.0.0
# RSPM@dplyr-1.0.0
# RSPM@R@4.0.1
# RSPM@R-4.0.1
#
# ## MRAN
#
# MRAN@2021-02-04T14:25:00Z
# MRAN@2021-02-04
# MRAN@dplyr@1.0.0
# MRAN@dplyr@1.0.0
# MRAN@R-4.0.1
# MRAN@R-4.0.1
#
# URLs
#
# https://cloud.r-project.org
#
# Paths (don't yet work with pkgcache)
#
# /Users/gaborcsardi/CRAN

repo_sugar <- function(x, nm) {
  psd <- parse_url(x)

  # URL
  if (!is.na(psd$protocol)) {
    repo_sugar_url(x, nm)

  } else if (grepl("^MRAN@", x)) {
    repo_sugar_mran(x, nm)

  } else if (grepl("^RSPM@", x)) {
    repo_sugar_rspm(x, nm)

  } else {
    repo_sugar_path(x, nm)
  }
}

repo_sugar_url <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "EXTRA"
  structure(x, names = nm)
}

repo_sugar_path <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "LOCAL"
  structure(x, names = nm)
}

repo_sugar_mran <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "CRAN"
  date <- parse_spec(sub("^MRAN@", "", x))
  if (date < "2015-02-01") {
    stop("MRAN snapshots go back to 2015-02-01 only")
  }

  mran <- Sys.getenv(
    "PKGCACHE_MRAN_URL",
    "https://cran.microsoft.com/snapshot"
  )
  structure(paste0(mran, "/", date), names = nm)
}

repo_sugar_rspm <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "CRAN"
  date <- parse_spec(sub("^RSPM@", "", x))
  if (is.null(pkgenv$rspm_versions) ||
      date < names(pkgenv$rspm_versions[1]) ||
      date > last(names(pkgenv$rspm_versions))) {
    tryCatch(
      pkgenv$rspm_versions <- get_rspm_versions(),
      error = function(err) warning("Failed to update list of RSPM versions")
    )
  }

  vers <- pkgenv$rspm_versions
  rspm_dates <- names(vers)
  if (date < rspm_dates[1]) {
    stop("RSPM snapshots go back to ", as.Date(rspm_dates[1]), " only")
  }
  sel <- which(date <= rspm_dates)[1]
  if (is.na(sel)) {
    stop("Cannot find matching RSPM snapshot for ", date)
  }

  rspm <- Sys.getenv(
    "PKGCACHE_RSPM_URL",
    "https://packagemanager.rstudio.com/all"
  )
  structure(paste0(rspm, "/", vers[[sel]]), names = nm)
}

get_rspm_versions <- function() {
  url <- Sys.getenv(
    "PKGCACHE_RSPM_TRANSACTIONS_URL",
    "https://packagemanager.rstudio.com/__api__/sources/1/transactions?_limit=10000"
  )
  resp <- jsonlite::fromJSON(url, simplifyVector = FALSE)

  vrs <- structure(
    vcapply(resp, function(x) as.character(x$id)),
    names = vcapply(resp, function(x) as.character(x$published_to))
  )
  vrs <- vrs[order(as.Date(names(vrs)))]
  vrs
}

parse_spec <- function(x) {
  if (grepl("^R[-@]", x)) {
    parse_spec_r(sub("^R[-@]", "", x))
  } else if (!is.na(at <- parse_iso_8601(x))) {
    parse_spec_date(at)
  } else {
    parse_spec_pkg(x)
  }
}

parse_spec_r <- function(x) {
  if (is.null(pkgenv$r_versions) ||
      package_version(x) > last(pkgenv$r_versions)$version) {
    tryCatch(
      pkgenv$r_versions <- get_r_versions(),
      error = function(err) warning("Failed to update list of R versions")
    )
  }

  vers <- vcapply(pkgenv$r_versions, "[[", "version")
  dates <- parse_iso_8601(vcapply(pkgenv$r_versions, "[[", "date"))

  if (! x %in% vers) {
    stop("Unknown R version: '", x, "'")
  }

  next_day(dates[match(x, vers)])
}

get_r_versions <- function() {
  url <- Sys.getenv(
    "PKGCACHE_R_VERSIONS_URL",
    "https://api.r-hub.io/rversions/r-versions"
  )
  res <- curl::curl_fetch_memory(url)
  if (res$status_code >= 300) {
    stop("Failed to download R versions from '", url, "'")
  }
  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
}

parse_spec_date <- function(at) {
  as.Date(at)
}

parse_spec_pkg <- function(x) {
  pkg <- sub("[-@].*$", "", x)
  ver <- sub("^.*[-@]", "", x)
  if (pkg == "" || ver == "") {
    stop("Invalid package version: '", x, "'")
  }

  if (is.null(pkgenv$pkg_versions[[pkg]]) ||
      package_version(ver) > last(names(pkgenv$pkg_versions[[pkg]]))) {
    pkgenv$pkg_versions[[pkg]] <- get_pkg_versions(pkg)
  }

  vers <- pkgenv$pkg_versions[[pkg]]
  if (! ver %in% names(vers)) {
    stop("Unknown '", pkg, "' version: '", ver, "'")
  }

  next_day(parse_iso_8601(vers[[ver]]))
}

get_pkg_versions <- function(pkg) {
  crandb <- Sys.getenv(
    "PKGCACHE_CRANDB_URL",
    "https://crandb.r-pkg.org/%s/all"
  )
  url <- sprintf(crandb, pkg)
  res <- curl::curl_fetch_memory(url)
  if (res$status_code == 404) {
    stop("Cannot find package versions for '", pkg, "'. Is it a CRAN package?")
  }
  if (res$status_code >= 300) {
    stop("Failed to download package versions for '", pkg, "' from '", url, "'")
  }

  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)$timeline
}

next_day <- function(x) {
  as.Date(x) + 1
}

#' @name repo_get
#' @rdname repo_get
#' @details
#' # Repository specifications
#'
#' The format of a repository specification is a named or unnamed
#' character scalar. If the name is missing, pkgcache adds a name
#' automatically. The repository named `CRAN` is the main CRAN repository,
#' but otherwise names are informational.
#'
#' Currently supported repository specifications:
#' - URL pointing to the root of the CRAN-like repository. Example:
#'   ```
#'   https://cloud.r-project.org
#'   ```
#' - `RSPM@<date>`, RSPM (RStudio Package Manager) snapshot, at the
#'   specified date.
#' - `RSPM@<package>-<version>` RSPM snapshot, for the day after the
#'   release of `<version>` of `<package>`.
#' - `RSPM@R-<version>` RSPM snapshot, for the day afer R `<version>`
#'   was released.
#' - `MRAN@<date>`, MRAN (Microsoft R Application Network) snapshot, at
#'   the specified date.
#' - `MRAN@<package>-<version>` MRAN snapshot, for the
#'   day after the release of `<version>` of `<package>`.
#' - `MRAN@R-<version>` MRAN snapshot, for the day
#'   afer R `<version>` was released.
#'
#'
#' Notes:
#' * See more about RSPM at <https://packagemanager.rstudio.com/client/#/>.
#' * See more about MRAN snapshots at
#'   <https://mran.microsoft.com/timemachine>.
#' * All dates (or times) can be specified in the ISO 8601 format.
#' * If RSPM does not have a snapshot available for a date, the next
#'   available date is used.
#' * Dates that are before the first, or after the last RSPM snapshot
#'   will trigger an error.
#' * Dates before the first, or after the last MRAN snapshot will trigger
#'   an error.
#' * Unknown R or package versions will trigger an error.
#'
NULL
