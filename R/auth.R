# Returns a set of HTTP headers for the given URL if (1) it belongs to a
# package repository; and (2) has credentials stored in the keyring.
repo_auth_headers <- function(url, allow_prompt = interactive()) {
  if (!grepl("/(src|bin)/", url)) {
    # Not a package or package index URL.
    return(NULL)
  }
  if (!requireNamespace("keyring", quietly = TRUE)) {
    return(NULL)
  }
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

  # In non-interactive contexts, force the use of the environment variable
  # backend so that we never hang but can still support CI setups.
  backend <- keyring::backend_env
  if (allow_prompt) {
    backend <- keyring::default_backend()
  }
  kb <- backend$new()

  # Use the repo URL without the username as the keyring "service".
  pwd <- NULL
  tryCatch(
    {
      pwd <- kb$get(creds$repourl, creds$username)
    },
    error = function(e) NULL
  )

  # Check whether we have one for the hostname as well.
  tryCatch(
    {
      pwd <- kb$get(creds$hosturl, creds$username)
    },
    error = function(e) NULL
  )

  if (is.null(pwd)) {
    return(NULL)
  }

  auth <- paste(creds$username, pwd, sep = ":")
  c("Authorization" = paste("Basic", base64_encode(auth)))
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
