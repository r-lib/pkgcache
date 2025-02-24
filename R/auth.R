#' Retrieve credentials for CRAN-like repos
#'
#' Returns a set of HTTP headers for the given URL if (1) it belongs to a
#' package repository; and (2) has credentials stored in the keyring.
#'
#' @param url Repo URL or download URL. For authentication it should include
#'   a username.
#' @param allow_prompt Whether to allow keyring backends that potentially
#'   require an interactive session.
#' @param use_cache Whether to allow retrieving the credenticals from the
#'   credential cache.
#' @param set_cache Whether to save the retrieved credentials in the
#'   credential cache.
#' @param parsed_url A shortcut for an already parsed URL. If not `NULL`
#'   then it should be the output of a `parse_url_basic_auth()` call.
#'   `url` is ignored in this case.
#' @return
#'   - `NULL` if the `url`` does not have authentication, e.g. if it does
#'     not include a (non-empty) username. It is also `NULL` if the `url`
#'     already has a password.
#'   - If `url` has authentication (but no password), then a list
#'     with entries:
#'     * `found`: `TRUE` if the function found the credentials, `FALSE`
#'       otherwise.
#'     * `headers`: character vector, the headers to add to the HTTP
#'       request.
#'     * `auth_domain`: the domain that was used to (try to) retrieve the
#'       credentials. This can be full path to the repository, with or
#'       without the username, or the hostname URL, with or without the
#'       username.
#'     * `source`: if the function found the credentials, then it is a
#'       short description about where the credencials were found.
#'     * `error`: if the function did not find the credentials, then it is
#'       a short description about why their retrieval failed.
#' @noRd

repo_auth_headers <- function(
  url,
  allow_prompt = interactive(),
  use_cache = TRUE,
  set_cache = TRUE,
  parsed_url = NULL) {

  # shortcut to speed up the common case of no credentials
  if (!grepl("@", url)) {
    return(NULL)
  }

  parsed_url <- parsed_url %||% parse_url_basic_auth(url)
  if (length(parsed_url$password) > 0 && nchar(parsed_url$password) != 0) {
    # The URL already contains a password. This is pretty poor practice, maybe
    # we should issue a warning pointing users to the keyring package instead.
    return(NULL)
  }
  if (length(parsed_url$username) == 0 || nchar(parsed_url$username) == 0) {
    # No username to key the lookup in the keyring with.
    return(NULL)
  }

  # Try URLs in this order:
  # - repo URL with username
  # - repo URL w/o username
  # - host URL with username
  # - host URL w/o username
  # We try each with and without a keyring username
  urls <- unique(unlist(
    parsed_url[c("repouserurl", "repourl", "hostuserurl", "hosturl")]
  ))

  if (use_cache) {
    for (u in urls) {
      if (u %in% names(pkgenv$credentials)) {
        creds <- pkgenv$credentials[[u]]
        creds$source <- paste0(creds$source, ":cached")
        return(creds)
      }
    }
  }

  res <- list(
    found = FALSE,
    headers = character(),
    auth_domain = urls[1],
    user = parsed_url$username,
    source = NULL,
    error = NULL
  )

  if (!requireNamespace("keyring", quietly = TRUE)) {
    res$found <- FALSE
    res$error <- "keyring not installed"
    return(res)
  }

  # In non-interactive contexts, force the use of the environment variable
  # backend so that we never hang but can still support CI setups.
  kb <- if (allow_prompt) {
    keyring::default_backend()
  } else {
    keyring::backend_env$new()
  }

  for (u in urls) {
    auth_domain <- u
    pwd <- try_catch_null(kb$get(u, parsed_url$username)) %||%
      try_catch_null(kb$get(u))
    if (!is.null(pwd)) break
  }

  if (!is.null(pwd)) {
    auth <- paste(parsed_url$username, pwd, sep = ":")
    res$found <- TRUE
    res$auth_domain <- auth_domain
    res$source <- paste0("keyring:", kb$name)
    res$headers <- c("Authorization" = paste("Basic", base64_encode(auth)))
  } else {
    res$error <- "keyring lookup failed"
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

parse_url_basic_auth <- function(url) {
  psd <- parse_url(url)
  if (is.na(psd$host)) {
    return(NULL)
  }
  userat <- if (nchar(psd$username)) paste0(psd$username, "@") else ""
  repo <- c(
    paste0(psd$protocol, "://", psd$host, psd$path),
    paste0(psd$protocol, "://", userat, psd$host, psd$path)
  )
  repo <- sub("(/(src|bin)/)(.*)$", "", repo)
  # Lop off any /__linux__/ subdirectories, too.
  repo <- sub("^(.*)/__linux__/[^/]+(/.*)$", "\\1\\2", repo, perl = TRUE)
  list(
    hosturl = paste0(psd$protocol, "://", psd$host),
    hostuserurl = paste0(psd$protocol, "://", userat, psd$host),
    repourl = repo[1],
    repouserurl = repo[2],
    username = psd$username,
    password = psd$password
  )
}
