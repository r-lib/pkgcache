# current_r_platform_linux

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/almalinux/8 
             "foo-almalinux-8.4" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
             fixtures/linux/alpine/3.11        fixtures/linux/alpine/3.12 
                   "foo-alpine-3.11.11"               "foo-alpine-3.12.7" 
             fixtures/linux/alpine/3.13        fixtures/linux/alpine/3.14 
                    "foo-alpine-3.13.5"               "foo-alpine-3.14.1" 
             fixtures/linux/alpine/edge 
      "foo-alpine-3.15.0_alpha20210804" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
            fixtures/linux/arch/base fixtures/linux/arch/base-devel 
                          "foo-arch"                     "foo-arch" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/centos/5 fixtures/linux/centos/6 fixtures/linux/centos/7 
            "foo-centos-5.11"       "foo-centos-6.10"          "foo-centos-7" 
      fixtures/linux/centos/8 
               "foo-centos-8" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
           fixtures/linux/debian/10      fixtures/linux/debian/11 
                    "foo-debian-10"               "foo-debian-11" 
            fixtures/linux/debian/8       fixtures/linux/debian/9 
                     "foo-debian-8"                "foo-debian-9" 
      fixtures/linux/debian/testing 
                    "foo-debian-11" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/fedora/32 fixtures/linux/fedora/33 fixtures/linux/fedora/34 
               "foo-fedora-32"          "foo-fedora-33"          "foo-fedora-34" 
      fixtures/linux/fedora/35 
               "foo-fedora-35" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
            fixtures/linux/opensuse/15.0       fixtures/linux/opensuse/15.1 
                "foo-opensuse-leap-15.0"           "foo-opensuse-leap-15.1" 
            fixtures/linux/opensuse/15.2       fixtures/linux/opensuse/15.3 
                "foo-opensuse-leap-15.2"           "foo-opensuse-leap-15.3" 
            fixtures/linux/opensuse/42.3 fixtures/linux/opensuse/tumbleweed 
                     "foo-opensuse-42.3" "foo-opensuse-tumbleweed-20210810" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/oraclelinux/6 fixtures/linux/oraclelinux/7 
                     "foo-ol-6.10"                 "foo-ol-7.9" 
      fixtures/linux/oraclelinux/8 
                      "foo-ol-8.4" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
        fixtures/linux/sl/6   fixtures/linux/sl/7 
      "foo-scientific-6.10"  "foo-scientific-7.9" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/ubuntu/14.04 fixtures/linux/ubuntu/16.04 
               "foo-ubuntu-14.04"          "foo-ubuntu-16.04" 
      fixtures/linux/ubuntu/18.04 fixtures/linux/ubuntu/20.04 
               "foo-ubuntu-18.04"          "foo-ubuntu-20.04" 
      fixtures/linux/ubuntu/21.04 fixtures/linux/ubuntu/21.10 
               "foo-ubuntu-21.04"          "foo-ubuntu-21.10" 

# default_cran_mirror

    Code
      withr::with_options(list(repos = NULL), default_cran_mirror())
    Output
                            CRAN 
      "https://cran.rstudio.com" 

---

    Code
      withr::with_options(list(repos = list(ACME = "https://acme.com")),
      default_cran_mirror())
    Output
      NULL

---

    Code
      withr::with_options(list(repos = c(ACME = "https://acme.com")),
      default_cran_mirror())
    Output
                            CRAN 
      "https://cran.rstudio.com" 

---

    Code
      withr::with_options(list(repos = list(CRAN = "@CRAN@")), default_cran_mirror())
    Output
                            CRAN 
      "https://cran.rstudio.com" 

---

    Code
      withr::with_options(list(repos = c(CRAN = "@CRAN@")), default_cran_mirror())
    Output
                            CRAN 
      "https://cran.rstudio.com" 

---

    Code
      withr::with_options(list(repos = list(CRAN = "https://mycran.com")),
      default_cran_mirror())
    Output
                      CRAN 
      "https://mycran.com" 

---

    Code
      withr::with_options(list(repos = c(CRAN = "https://mycran.com")),
      default_cran_mirror())
    Output
                      CRAN 
      "https://mycran.com" 

# bioc_version

    Code
      bioc_version("4.1.1")
    Output
      [1] ‘3.13’
    Code
      bioc_version("4.0.0")
    Output
      [1] ‘3.12’
    Code
      bioc_version("3.6.0")
    Output
      [1] ‘3.10’

# bioc_version_map

    Code
      bioc_version_map()
    Output
      [38;5;246m# A tibble: 35 × 3[39m
         bioc_version r_version  bioc_status
         [3m[38;5;246m<pckg_vrs>[39m[23m   [3m[38;5;246m<pckg_vrs>[39m[23m [3m[38;5;246m<fct>[39m[23m      
      [38;5;250m 1[39m 1.6          2.1        out-of-date
      [38;5;250m 2[39m 1.7          2.2        out-of-date
      [38;5;250m 3[39m 1.8          2.3        out-of-date
      [38;5;250m 4[39m 1.9          2.4        out-of-date
      [38;5;250m 5[39m 2.0          2.5        out-of-date
      [38;5;250m 6[39m 2.1          2.6        out-of-date
      [38;5;250m 7[39m 2.2          2.7        out-of-date
      [38;5;250m 8[39m 2.3          2.8        out-of-date
      [38;5;250m 9[39m 2.4          2.9        out-of-date
      [38;5;250m10[39m 2.5          2.10       out-of-date
      [38;5;246m# … with 25 more rows[39m

# bioc_release_version, bioc_devel_version

    Code
      bioc_release_version()
    Output
      [1] ‘3.13’

---

    Code
      bioc_devel_version()
    Output
      [1] ‘3.14’

# bioc_repos

    Code
      bioc_repos("3.13")
    Output
                                                      BioCsoft 
                 "https://bioconductor.org/packages/3.13/bioc" 
                                                       BioCann 
      "https://bioconductor.org/packages/3.13/data/annotation" 
                                                       BioCexp 
      "https://bioconductor.org/packages/3.13/data/experiment" 
                                                 BioCworkflows 
            "https://bioconductor.org/packages/3.13/workflows" 

