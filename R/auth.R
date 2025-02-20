# Returns a set of HTTP headers for the given URL if (1) it belongs to a
# package repository; and (2) has credentials stored in the keyring.
repo_auth_headers <- function(
  url, allow_prompt = interactive(), use_cache = TRUE, set_cache = TRUE) {

  creds <- extract_basic_auth_credentials(url)
  if (length(creds$password) > 0 && nchar(creds$password) != 0) {
    # The URL already contains a password. This is pretty poor practice, maybe
    # we should issue a warning pointing users to the keyring package instead.
    return(NULL)
  }
  if (length(creds$username) == 0 || nchar(creds$username) == 0) {
    # No username to key the lookup in the keyring with.
    return(NULL)
  }

  if (use_cache) {
    if (creds$repourl %in% names(pkgenv$credentials)) {
      return(pkgenv$credentials[[creds$repourl]])
    }
    if (creds$hosturl %in% names(pkgenv$credentials)) {
      return(pkgenv$credentials[[creds$hosturl]])
    }
  }

  if (!requireNamespace("keyring", quietly = TRUE)) {
    return(NULL)
  }

  # In non-interactive contexts, force the use of the environment variable
  # backend so that we never hang but can still support CI setups.
  kb <- if (allow_prompt) {
    keyring::default_backend()
  } else {
    keyring::backend_env$new()
  }

  # First use the repo URL without the username as the keyring "service".
  auth_domain <- creds$repourl
  pwd <- try_catch_null(kb$get(creds$repourl, creds$username))

  # Check whether we have one for the hostname as well.
  if (is.null(pwd)) {
    auth_domain <- creds$hosturl
    pwd <- try_catch_null(kb$get(creds$hosturl, creds$username))
  }

  res <- if (!is.null(pwd)) {
    auth <- paste(creds$username, pwd, sep = ":")
    list(
      headers = c("Authorization" = paste("Basic", base64_encode(auth))),
      auth_domain = auth_domain
    )
  }

  if (set_cache) {
    pkgenv$credentials[[auth_domain]] <- res
  }

  res
}

clear_auth_cache <- function(key = NULL) {
  if (is.null(key) ||
    identical(pkgenv$credentials[[".exit_handler"]], key)) {
    rm(
      list = ls(pkgenv$credentials, all.names = TRUE),
      envir = pkgenv$credentials
    )
  }
}

start_auth_cache <- function(key) {
  if (! ".exit_handler" %in% names(pkgenv$credentials)) {
    assign(".exit_handler", key, envir = pkgenv$credentials)
  }
}

base64_encode <- function(x) {
  if (!is.raw(x)) {
    x <- charToRaw(x)
  }
  processx::base64_encode(x)
}

extract_basic_auth_credentials <- function(url) {
  psd <- parse_url(url)
  if (is.na(psd$host)) {
    throw(new_error(cli::format_error(
      "Unrecognized URL format: {.code {url}}."
    )))
  }
  # ideally we would work with the repo URL, and not the final download URL
  # until then, we strip the download URL to get the repo URL
  repo <- paste0(psd$protocol, "://", psd$host, psd$path)
  repo <- sub("(/(src|bin)/)(.*)$", "", repo)
  # Lop off any /__linux__/ subdirectories, too.
  repo <- sub("^(.*)/__linux__/[^/]+(/.*)$", "\\1\\2", repo, perl = TRUE)
  list(
    hosturl = paste0(psd$protocol, "://", psd$host),
    repourl = repo,
    username = psd$username,
    password = psd$password
  )
}
