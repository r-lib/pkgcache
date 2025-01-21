# Returns a set of HTTP headers for the given URL if (1) it belongs to a
# package repository; and (2) has credentials stored in the keyring.
repo_auth_headers <- function(url, allow_prompt = interactive()) {
  if (!grepl("/(src|bin)/", url)) {
    # Not a package or package index URL.
    return(NULL)
  }
  if (!rlang::is_installed("keyring")) {
    return(NULL)
  }
  creds <- extract_basic_auth_credentials(url)
  if (!is.null(creds$password)) {
    # The URL already contains a password. This is pretty poor practice, maybe
    # we should issue a warning pointing users to the keyring package instead.
    return(NULL)
  }
  if (is.null(creds$username)) {
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
  svc <- extract_repo_url(url)
  pwd <- NULL
  tryCatch(
    {
      pwd <- kb$get(svc, creds$username)
    },
    error = function(e) NULL
  )

  # Check whether we have one for the hostname as well.
  svc <- extract_hostname(url)
  tryCatch(
    {
      pwd <- kb$get(svc, creds$username)
    },
    error = function(e) NULL
  )

  if (is.null(pwd)) {
    return(NULL)
  }

  auth <- paste(creds$username, pwd, sep = ":")
  c("Authorization" = paste("Basic", processx::base64_encode(auth)))
}

extract_basic_auth_credentials <- function(url) {
  pattern <- "^https?://(?:([^:@/]+)(?::([^@/]+))?@)?.*$"
  if (!grepl(pattern, url, perl = TRUE)) {
    cli::cli_abort("Unrecognized URL format: {.url {url}}.", .internal = TRUE)
  }
  username <- sub(pattern, "\\1", url, perl = TRUE)
  if (!nchar(username)) {
    username <- NULL
  }
  password <- sub(pattern, "\\2", url, perl = TRUE)
  if (!nchar(password)) {
    password <- NULL
  }
  list(username = username, password = password)
}

extract_repo_url <- function(url) {
  url <- sub(
    "^(https?://)(?:[^:@/]+(?::[^@/]+)?@)?(.*)(/(src|bin)/)(.*)$",
    "\\1\\2",
    url,
    perl = TRUE
  )
  # Lop off any /__linux__/ subdirectories, too.
  sub("^(.*)/__linux__/[^/]+(/.*)$", "\\1\\2", url, perl = TRUE)
}

extract_hostname <- function(url) {
  sub(
    "^(https?://)(?:[^:@/]+(?::[^@/]+)?@)?([^/]+)(.*)",
    "\\1\\2",
    url,
    perl = TRUE
  )
}
