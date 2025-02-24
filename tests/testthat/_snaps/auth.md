# looking up auth headers for repositories works as expected

    Code
      repo_auth_headers("https://username@ppm.internal/healthz", use_cache = FALSE,
        set_cache = FALSE)
    Output
      $found
      [1] FALSE
      
      $headers
      character(0)
      
      $auth_domain
      [1] "https://username@ppm.internal/healthz"
      
      $username
      [1] "username"
      
      $source
      NULL
      
      $error
      [1] "keyring lookup failed"
      

---

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE, use_cache = FALSE, set_cache = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      

---

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE, use_cache = FALSE, set_cache = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      

# without keyring

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE, use_cache = FALSE, set_cache = FALSE)
    Output
      $found
      [1] FALSE
      
      $headers
      character(0)
      
      $auth_domain
      [1] "https://username@ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      NULL
      
      $error
      [1] "keyring not installed"
      

# caching

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      
    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/__linux__/jammy/latest/src/contrib/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env:cached"
      
      $error
      NULL
      
    Code
      pkgenv$credentials[["https://ppm.internal/cran/latest"]]
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal/cran/latest"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      

---

    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      
    Code
      repo_auth_headers(
        "https://username@ppm.internal/cran/latest/bin/linux/4.4-jammy/contrib/4.4/PACKAGES.gz",
        allow_prompt = FALSE)
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env:cached"
      
      $error
      NULL
      
    Code
      pkgenv$credentials[["https://ppm.internal"]]
    Output
      $found
      [1] TRUE
      
      $headers
                     Authorization 
      "Basic dXNlcm5hbWU6dG9rZW4=" 
      
      $auth_domain
      [1] "https://ppm.internal"
      
      $username
      [1] "username"
      
      $source
      [1] "keyring:env"
      
      $error
      NULL
      

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

# repo with basic auth

    Code
      cmc$update()
    Message
      v Updated metadata database: <size> <unit> in <num> file<s>.
      i source packages are missing from CRAN: Unauthorized (HTTP 401).
      i Updating metadata database
      v Updating metadata database ... done
    Output
      $pkgs
      # A data frame: 0 x 20
      # i 20 variables: repodir <chr>, rversion <chr>, platform <chr>,
      #   needscompilation <chr>, priority <chr>, package <chr>, version <chr>,
      #   ref <chr>, type <chr>, direct <lgl>, status <chr>, target <chr>,
      #   mirror <chr>, sources <list>, filesize <lgl>, sha256 <chr>, sysreqs <lgl>,
      #   built <chr>, published <dttm>, deps <list>
      $deps
      # A data frame: 0 x 7
      # i 7 variables: upstream <chr>, idx <int>, ref <chr>, type <chr>,
      #   package <chr>, version <chr>, op <chr>

---

    Code
      cmc$update()
    Message
      v Updated metadata database: <size> <unit> in <num> file<s>.
      i Updating metadata database
      v Updating metadata database ... done
    Output
      $pkgs
      # A data frame: 3 x 22
        package version md5sum      needscompilation depends repodir rversion platform
        <chr>   <chr>   <chr>       <chr>            <chr>   <chr>   <chr>    <chr>   
      1 pkg1    1.0.0   <md5sum> no               <NA>    src/co~ *        source  
      2 pkg2    1.0.0   <md5sum> no               pkg1    src/co~ *        source  
      3 pkg3    1.0.0   <md5sum> no               pkg2    src/co~ *        source  
      # i 14 more variables: priority <chr>, ref <chr>, type <chr>, direct <lgl>,
      #   status <chr>, target <chr>, mirror <chr>, sources <list>, filesize <int>,
      #   sha256 <chr>, sysreqs <chr>, built <chr>, published <dttm>, deps <list>
      $deps
      # A data frame: 2 x 7
        upstream   idx ref   type    package op    version
        <chr>    <int> <chr> <chr>   <chr>   <chr> <chr>  
      1 pkg2         2 pkg1  depends pkg1    ""    ""     
      2 pkg3         3 pkg2  depends pkg2    ""    ""     

# basic auth credentials can be extracted from various URL formats

    Code
      parse_url_basic_auth("https://user.name:pass-word123@example.com")
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
      parse_url_basic_auth("http://user@example.com")
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
      parse_url_basic_auth("https://example.com")
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
      

