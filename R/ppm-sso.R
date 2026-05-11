ppm_sso_login <- function(
  ppm_url = Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
) {
  identity_token <- ppm_sso_get_identity_token_from_file() %||%
    ppm_sso_device_flow(ppm_url)
  ppm_token <- ppm_sso_identity_to_ppm_token(ppm_url, identity_token)
  ppm_sso_write_token_to_file(ppm_url, ppm_token)
  ppm_token
}

ppm_sso_auth <- function(repo) {
  ppm_url <- Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
  if (is.na(ppm_url)) {
    stop(
      "Please set the PACKAGEMANAGER_ADDRESS environment variable to ",
      "the URL of your RStudio Package Manager instance."
    )
  }

  parsed <- tryCatch(
    parse_url(repo),
    error = function(e) {
      stop("Failed to parse repository URL: ", repo)
    }
  )
  repo_host <- paste0(parsed$protocol, "://", parsed$host)
  if (repo_host != ppm_url) {
    stop(
      "The repository URL (",
      repo_host,
      ") does not match the configured ",
      "Package Manager URL (",
      ppm_url,
      ")."
    )
  }

  ppm_sso_get_existing_token(ppm_url, valid = TRUE) %||% ppm_sso_login(ppm_url)
}

ppm_sso_post_form <- function(url, payload) {
  payload <- payload[!vapply(payload, is.null, logical(1))]
  body <- paste(
    paste0(
      curl::curl_escape(names(payload)),
      "=",
      curl::curl_escape(unlist(payload, use.names = FALSE))
    ),
    collapse = "&"
  )
  h <- curl::new_handle()
  curl::handle_setheaders(
    h,
    "Content-Type" = "application/x-www-form-urlencoded"
  )
  curl::handle_setopt(h, post = TRUE, postfields = body)
  resp <- curl::curl_fetch_memory(url, handle = h)
  list(
    status = resp$status_code,
    body = jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  )
}

ppm_sso_token_path <- function() {
  file.path(
    path.expand("~"),
    ".ppm",
    "tokens.toml"
  )
}

ppm_sso_get_existing_token <- function(ppm_url, valid = TRUE) {
  path <- ppm_sso_token_path()
  try_catch_null({
    ts_tokens <- suppressWarnings(tstoml::ts_read_toml(path))
    for (conn in ts_tokens[[list("connection", TRUE)]]) {
      if (identical(conn$url, ppm_url)) {
        if (valid && !ppm_sso_can_authenticate(ppm_url, conn$token)) {
          return(NULL)
        }
        return(conn$token)
      }
    }
  })
}

ppm_sso_get_identity_token_from_file <- function() {
  token_file <- Sys.getenv("PACKAGEMANAGER_IDENTITY_TOKEN_FILE", unset = NA)
  if (is.na(token_file)) {
    return(NULL)
  }
  try_catch_null({
    trimws(readLines(token_file, n = 1, warn = FALSE))
  })
}

ppm_sso_device_flow <- function(ppm_url) {
  verifier <- ppm_sso_new_pkce_verifier()
  challenge <- ppm_sso_new_pkce_challenge(verifier)

  # 1. Initiate Device Auth
  init_url <- paste0(ppm_url, "/__api__/device")
  payload <- list(
    code_challenge_method = "S256",
    code_challenge = challenge
  )
  init_resp <- ppm_sso_post_form(init_url, payload)
  if (init_resp$status >= 400) {
    stop(
      "Failed to initiate device authorization (HTTP ",
      init_resp$status,
      ")."
    )
  }
  init_resp_body <- init_resp$body

  display_uri <- init_resp_body$verification_uri_complete %||%
    init_resp_body$verification_uri
  if (is.null(display_uri)) {
    stop("No verification URI found in device auth response.")
  }

  message("\nPlease open the following URL in your browser:")
  message(paste("  ", display_uri))
  message("\nAnd enter the following code when prompted:")
  message(paste("  ", init_resp_body$user_code))
  message("\nWaiting for authorization...")

  try(utils::browseURL(display_uri), silent = TRUE)

  # 2. Poll for token
  token_resp_body <- ppm_sso_complete_device_auth(
    ppm_url,
    init_resp_body$device_code,
    verifier,
    init_resp_body$interval %||% 5,
    init_resp_body$expires_in %||% 300
  )

  if (is.null(token_resp_body) || is.null(token_resp_body$id_token)) {
    stop("Failed to complete device authorization or obtain identity token.")
  }

  token_resp_body$id_token
}

ppm_sso_can_authenticate <- function(ppm_url, token) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Authorization" = paste("Bearer", token))
  resp <- curl::curl_fetch_memory(ppm_url, handle = h)
  status <- resp$status_code
  status < 500 && status != 401 && status != 403
}

ppm_sso_identity_to_ppm_token <- function(ppm_url, identity_token) {
  url <- paste0(ppm_url, "/__api__/token")
  payload <- list(
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token = identity_token,
    subject_token_type = "urn:ietf:params:oauth:token-type:id_token"
  )

  resp <- ppm_sso_post_form(url, payload)
  if (resp$status >= 400) {
    stop(
      "Failed to exchange identity token for PPM token (HTTP ",
      resp$status,
      ")."
    )
  }

  token_data <- resp$body
  if (is.null(token_data$access_token)) {
    stop("Failed to exchange identity token for PPM token.")
  }

  token_data$access_token
}

ppm_sso_write_token_to_file <- function(ppm_url, token) {
  # this is more difficult than it should be because TOML is unable
  # to represent an empty array of tables
  token_file_path <- ppm_sso_token_path()
  mkdirp(dirname(token_file_path))
  new_conn <- list(
    url = ppm_url,
    token = token,
    method = "sso"
  )

  tokens <- try_catch_null({
    tokens <- suppressWarnings(tstoml::ts_read_toml(token_file_path))
    urls <- ts::ts_tree_unserialize(
      ts::ts_tree_select(tokens, list("connection", TRUE, "url"))
    )
    idx <- which(urls == ppm_url)[1]
    tokens
  })

  if (is.null(tokens)) {
    tokens <- tstoml::ts_parse_toml("")
    tokens <- ts::ts_tree_insert(tokens, key = "connection", list(new_conn))
  } else if (!is.na(idx)) {
    tokens <- ts::ts_tree_update(
      ts::ts_tree_select(tokens, list("connection", idx, "token")),
      new_conn$token
    )
  } else {
    tokens <- ts::ts_tree_insert(
      ts::ts_tree_select(tokens, "connection"),
      list(new_conn)
    )
  }

  ts::ts_tree_write(tokens, token_file_path)
}

ppm_sso_base64url_encode <- function(x) {
  encoded <- processx::base64_encode(x)
  # Make it URL-safe
  gsub("\\+", "-", gsub("\\/", "_", gsub("=+$", "", encoded)))
}

ppm_sso_hex_to_raw <- function(s) {
  n <- nchar(s)
  as.raw(strtoi(substring(s, seq(1L, n, 2L), seq(2L, n, 2L)), 16L))
}

ppm_sso_sha256_raw <- function(x) {
  ppm_sso_hex_to_raw(cli::hash_sha256(x))
}

ppm_sso_new_pkce_verifier <- function() {
  ppm_sso_base64url_encode(.Call(pkgcache_rand_bytes, 32L))
}

ppm_sso_new_pkce_challenge <- function(verifier) {
  ppm_sso_base64url_encode(ppm_sso_sha256_raw(verifier))
}

ppm_sso_complete_device_auth = function(
  ppm_url,
  device_code,
  verifier,
  interval,
  expires_in
) {
  url <- paste0(ppm_url, "/__api__/device_access")
  start_time <- Sys.time()
  payload <- list(
    device_code = device_code,
    code_verifier = verifier
  )

  while (as.numeric(Sys.time() - start_time) < expires_in) {
    resp <- ppm_sso_post_form(url, payload)
    status <- resp$status

    if (status == 200) {
      return(resp$body)
    } else if (status == 400) {
      error_code <- resp$body$error
      if (error_code == "access_denied") {
        stop("Access denied by user.")
      }
      if (error_code == "expired_token") {
        stop("Device authorization request expired.")
      }
      # For "authorization_pending" or "slow_down", just wait and retry.
    } else {
      stop("Device authorization failed (HTTP ", status, ").")
    }

    Sys.sleep(interval)
  }

  stop("Device authorization timed out.")
}
