#' Authenticated repositories
#'
#' pkgcache supports HTTP basic authentication when interacting with
#' CRAN-like repositories. To user authentication, include a username
#' in the repo URL:
#' ```
#' https://<username>@<repo-host>/<repo-path>
#' ```
#'
#' pkgcache will look up password for this url and username from the
#' system credential store using the keyring package. For the URL above
#' it tries the following keyring keys, in this order:
#' ```
#' https://<username>@repo-host/<repo-path>
#' https://repo-host/<repo-path>
#' https://<username>@repo-host
#' https://repo-host
#' ```
#'
#' To add an authenticated repository use [repo_add()] with the `username`
#' argument. Alternatively, you can set the `repos` option directly using
#' [base::options()] and including the username in the repository URL.
#'
#' `repo_auth()` lists authentication information for all configured
#' repositories.
#'
#' @inheritParams repo_get
#' @param check_credentials Whether to check that credentials are
#'   available for authenticated repositories.
#' @return Data frame with columns:
#'   - all columns from the output of [repo_get()],
#'   - `auth_domains`: authentication domains. pkgcache tries to find a
#'     credential for these domains, until the search is successful or all
#'     domains fail.
#'   - `auth_domain`: if the credential lookup is successful, then this is
#'     the authentication domain that was used to get the credential.
#'   - `auth_source`: where the credential was found. E.g.
#'     `keyring:<backend>` means it was in the default macos keyring.
#'   - `auth_error`: for failed credential searches this is the description
#'     of why the search failed. E.g. maybe the keyring package is not
#'     installed, or pkgcache found no credentials for any of the
#'     authentication domains.
#'
#' @export

repo_auth <- function(r_version = getRversion(), bioc = TRUE,
                      cran_mirror = default_cran_mirror(),
                      check_credentials = TRUE) {
  res <- cmc__get_repos(
    getOption("repos"),
    bioc = bioc,
    cran_mirror = cran_mirror,
    as.character(r_version),
    auth = FALSE
  )

  res$username <- rep(NA_character_, nrow(res))
  res$has_password <- rep(NA, nrow(res))
  res$auth_domains <- I(replicate(nrow(res), NULL))
  res$auth_domain <- rep(NA_character_, nrow(res))
  res$auth_source <- rep(NA_character_, nrow(res))
  res$auth_error <- rep(NA_character_, nrow(res))
  for (w in seq_len(nrow(res))) {
    url <- res$url[w]
    cred <- repo_auth_headers(url, warn = FALSE)
    if (is.null(cred)) next
    res$username[w] <- cred$username
    res$has_password[w] <- cred$found
    res$auth_domains[w] <- list(cred$auth_domains)
    if (cred$found) {
      res$auth_source[w] <- cred$source
      res$auth_domain[w] <- cred$auth_domain
    } else {
      res$auth_error[w] <- cred$error
    }
  }

  res
}

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
#' @param warn Whether to warn if the function cannot find credentials.
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
#'     * `auth_domains`: all possible authentication domains.
#'     * `auth_domain`: the domain that was used to retrieve the
#'       credentials. This can be full path to the repository, with or
#'       without the username, or the hostname URL, with or without the
#'       username.
#'     * `username`: user name, from the URL.
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
  warn = TRUE) {

  # shortcut to speed up the common case of no credentials
  if (!grepl("@", url)) {
    return(NULL)
  }

  parsed_url <- parse_url_basic_auth(url)
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
    auth_domains = urls,
    auth_domain = NA_character_,
    username = parsed_url$username,
    source = NULL,
    error = NULL
  )

  if (!requireNamespace("keyring", quietly = TRUE)) {
    res$found <- FALSE
    res$error <- "keyring not installed"
    if (warn) {
      cli::cli_alert_warning(
        "Cannot find credentials for URL {.url {url}}, the keyring package
         is not installed."
      )
    }
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
    if (warn) {
      cli::cli_alert_warning(
        "Cannot find credentials for URL {.url {url}}, credential lookup
         failed. Keyring backend: {.val {kb$name}}."
      )
    }
    res$error <- paste0("keyring lookup failed (", kb$name, " backend)")
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

add_auth_status <- function(repos) {
  maybe_has_auth <- grepl("^https?://[^/]*@", repos$url)
  if (!any(maybe_has_auth)) return(repos)

  key <- random_key()
  on.exit(clear_auth_cache(key), add = TRUE)
  start_auth_cache(key)

  repos$username <- rep(NA_character_, nrow(repos))
  repos$has_password <- rep(NA, nrow(repos))
  for (w in which(maybe_has_auth)) {
    url <- repos$url[w]
    creds <- repo_auth_headers(url, warn = FALSE)
    if (is.null(creds)) next
    repos$username[w] <- creds$username
    repos$has_password[w] <- creds$found
  }

  repos
}
