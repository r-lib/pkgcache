
async_get_rspm_versions <- function(forget = FALSE, date = NULL) {
  tmp1 <- tempfile()
  def <- if (forget ||
             (!is.null(date) && date < names(pkgenv$rspm_versions[1])) ||
             (!is.null(date) && date > last(names(pkgenv$rspm_versions)))) {
    url <- Sys.getenv(
      "PKGCACHE_RSPM_TRANSACTIONS_URL",
      "https://packagemanager.posit.co/__api__/sources/1/transactions?_limit=10000"
    )
    tmp <- tempfile()
    download_file(url, tmp1)$
      then(function(res) {
        resp <- jsonlite::fromJSON(tmp1, simplifyVector = FALSE)
        vrs <- structure(
          vcapply(resp, function(x) as.character(x$id)),
          names = vcapply(resp, function(x) as.character(x$published_to))
        )
        pkgenv$rspm_versions <- vrs[order(as.Date(names(vrs)))]
      })$
      catch(error = function() {
        warning("Failed to download RSPM versions")
      })

  } else {
    async_constant()
  }

  def$
    finally(function() unlink(tmp1))$
    then(function() pkgenv$rspm_versions)
}

async_get_rspm_distros <- function(forget = FALSE, distribution = NULL,
                                   release = NULL) {
  tmp2 <- tempfile()

  # is this a known distro?
  known <- if (is.null(distribution)) {
    TRUE
  } else if (is.null(release)) {
    distribution %in% pkgenv$rspm_distros_cached$distribution
  } else {
    mch <- which(
      distribution == pkgenv$rspm_distros_cached$distribution &
      release == pkgenv$rspm_distros_cached$release
    )
    !is.na(mch)
  }

  # can we used the cached values? Only if
  # * not a forced update, and
  # * distro is known, or we already updated.
  updated <- !is.null(pkgenv$rspm_distros)
  cached <- !forget && (known || updated)
  def <- if (cached) {
    pkgenv$rspm_distros <- pkgenv$rspm_distros_cached
    async_constant()
  } else {
    url <- Sys.getenv(
      "PKGCACHE_RSPM_STATUS_URL",
      "https://packagemanager.posit.co/__api__/status"
    )
    download_file(url, tmp2)$
      then(function(res) {
        stat <- jsonlite::fromJSON(tmp2, simplifyVector = FALSE)
        dst <- data.frame(
          stringsAsFactors = FALSE,
          name = vcapply(stat$distros, "[[", "name"),
          os = vcapply(stat$distros, "[[", "os"),
          binary_url = vcapply(stat$distros, "[[", "binaryURL"),
          distribution = vcapply(stat$distros, "[[", "distribution"),
          release = vcapply(stat$distros, "[[", "release"),
          binaries = vlapply(stat$distros, "[[", "binaries")
        )
        pkgenv$rspm_distros <- dst
        pkgenv$rspm_distros_cached <- dst
      })$
      catch(error = function() {
        warning("Failed to download RSPM status")
      })
  }

  def$
    finally(function() unlink(tmp2))$
    then(function() pkgenv$rspm_distros)
}

parse_rspm_bintag <- function(bintag) {
  if (length(bintag) == 0) {
    warning("No 'x-package-binary-tag' header from RSPM.")
    return(list(
      platform = current_r_platform(),
      r_version = get_minor_r_version(getRversion())
    ))
  }
  bintag <- sub("^x-package-binary-tag: ?", "", bintag[1])

}
