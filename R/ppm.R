
get_ppm_url <- function() {
  ppm_env <- Sys.getenv("PKGCACHE_PPM_URL")
  if (ppm_env != "") {
    paste0(
      ppm_env,
      "/",
      Sys.getenv("PKGCACHE_PPM_REPO", "cran")
    )
  } else {
    Sys.getenv(
      "PKGCACHE_RSPM_URL",
      "https://packagemanager.posit.co/cran"
    )
  }
}

get_ppm_base_url <- function() {
  dirname(get_ppm_url())
}

async_get_ppm_versions <- function(forget = FALSE, date = NULL) {
  tmp1 <- tempfile()
  def <- if (forget ||
             (!is.null(date) && date < names(pkgenv$ppm_versions[1])) ||
             (!is.null(date) && date > last(names(pkgenv$ppm_versions)))) {
    url <- Sys.getenv(
      "PKGCACHE_PPM_TRANSACTIONS_URL",
      paste0(get_ppm_base_url(), "/__api__/sources/1/transactions?_limit=10000")
    )
    tmp <- tempfile()
    download_file(url, tmp1)$
      then(function(res) {
        resp <- jsonlite::fromJSON(tmp1, simplifyVector = FALSE)
        vrs <- structure(
          vcapply(resp, function(x) as.character(x$id)),
          names = vcapply(resp, function(x) as.character(x$published_to))
        )
        pkgenv$ppm_versions <- vrs[order(as.Date(names(vrs)))]
      })$
      catch(error = function(err) {
        warning("Failed to download PPM versions")
      })

  } else {
    async_constant()
  }

  def$
    finally(function() unlink(tmp1))$
    then(function() pkgenv$ppm_versions)
}

async_get_ppm_distros <- function(forget = FALSE, distribution = NULL,
                                   release = NULL) {
  tmp2 <- tempfile()

  # is this a known distro?
  known <- if (is.null(distribution)) {
    TRUE
  } else if (is.null(release)) {
    distribution %in% pkgenv$ppm_distros_cached$distribution
  } else {
    mch <- which(
      distribution == pkgenv$ppm_distros_cached$distribution &
      release == pkgenv$ppm_distros_cached$release
    )
    !is.na(mch)
  }

  # can we used the cached values? Only if
  # * not a forced update, and
  # * distro is known, or we already updated.
  updated <- !is.null(pkgenv$ppm_distros)
  cached <- !forget && (known || updated)
  def <- if (cached) {
    pkgenv$ppm_distros <- pkgenv$ppm_distros_cached
    async_constant()
  } else {
    url <- Sys.getenv(
      "PKGCACHE_PPM_STATUS_URL",
      paste0(get_ppm_base_url(), "/__api__/status")
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
        pkgenv$ppm_distros <- dst
        pkgenv$ppm_distros_cached <- dst
      })$
      catch(error = function(err) {
        warning("Failed to download PPM status")
      })
  }

  def$
    finally(function() unlink(tmp2))$
    then(function() pkgenv$ppm_distros)
}
