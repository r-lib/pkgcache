# looking up auth headers for repositories works as expected

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE, use_cache = FALSE, set_cache = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      

---

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE, use_cache = FALSE, set_cache = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      

# caching

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
    Code
      pkgenv$credentials[["https://ppm.internal/cran/latest"]]
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      

---

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      
    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      
    Code
      pkgenv$credentials[["https://ppm.internal"]]
    Output
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      

# http requests with auth

    Code
      readLines(tmp, warn = FALSE)
    Output
      [1] "{\"authenticated\":true,\"user\":\"username\"}"

---

    Code
      readLines(tmp, warn = FALSE)
    Output
      [1] "{\"authenticated\":true,\"user\":\"username\"}"

---

    Code
      readLines(tmp2, warn = FALSE)
    Output
      [1] "{\"authenticated\":true,\"user\":\"username\"}"

---

    Code
      synchronise(download_file(url2, tmp3))
    Condition
      Error in `stop()`:
      ! Unauthorized (HTTP 401).

---

    Code
      synchronise(download_if_newer(url2, tmp3))
    Condition
      Error in `stop()`:
      ! Unauthorized (HTTP 401).

# basic auth credentials can be extracted from various URL formats

    Code
      extract_basic_auth_credentials("https://user.name:pass-word123@example.com")
    Output
      $hosturl
      [1] "https://example.com"
      
      $hostuserurl
      [1] "https://user.name@example.com"
      
      $repourl
      [1] "https://example.com"
      
      $repouserurl
      [1] "https://user.name@example.com"
      
      $username
      [1] "user.name"
      
      $password
      [1] "pass-word123"
      
    Code
      extract_basic_auth_credentials("http://user@example.com")
    Output
      $hosturl
      [1] "http://example.com"
      
      $hostuserurl
      [1] "http://user@example.com"
      
      $repourl
      [1] "http://example.com"
      
      $repouserurl
      [1] "http://user@example.com"
      
      $username
      [1] "user"
      
      $password
      [1] ""
      
    Code
      extract_basic_auth_credentials("https://example.com")
    Output
      $hosturl
      [1] "https://example.com"
      
      $hostuserurl
      [1] "https://example.com"
      
      $repourl
      [1] "https://example.com"
      
      $repouserurl
      [1] "https://example.com"
      
      $username
      [1] ""
      
      $password
      [1] ""
      

---

    Code
      extract_basic_auth_credentials("notaurl")
    Condition
      Error:
      ! Unrecognized URL format: `notaurl`.

