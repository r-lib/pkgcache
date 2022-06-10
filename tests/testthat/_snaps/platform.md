# current_r_platform_data_linux

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/almalinux/8`
        raw distribution release
      1 foo    almalinux     8.4
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/alpine/3.11`
        raw distribution release
      1 foo       alpine 3.11.11
      
      $`fixtures/linux/alpine/3.12`
        raw distribution release
      1 foo       alpine  3.12.7
      
      $`fixtures/linux/alpine/3.13`
        raw distribution release
      1 foo       alpine  3.13.5
      
      $`fixtures/linux/alpine/3.14`
        raw distribution release
      1 foo       alpine  3.14.1
      
      $`fixtures/linux/alpine/edge`
        raw distribution              release
      1 foo       alpine 3.15.0_alpha20210804
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/arch/base`
        raw distribution
      1 foo         arch
      
      $`fixtures/linux/arch/base-devel`
        raw distribution
      1 foo         arch
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/centos/5`
        raw distribution release
      1 foo       centos    5.11
      
      $`fixtures/linux/centos/6`
        raw distribution release
      1 foo       centos    6.10
      
      $`fixtures/linux/centos/7`
        raw distribution release
      1 foo       centos       7
      
      $`fixtures/linux/centos/8`
        raw distribution release
      1 foo       centos       8
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/debian/10`
        raw distribution release
      1 foo       debian      10
      
      $`fixtures/linux/debian/11`
        raw distribution release
      1 foo       debian      11
      
      $`fixtures/linux/debian/8`
        raw distribution release
      1 foo       debian       8
      
      $`fixtures/linux/debian/9`
        raw distribution release
      1 foo       debian       9
      
      $`fixtures/linux/debian/testing`
        raw distribution release
      1 foo       debian      11
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/fedora/32`
        raw distribution release
      1 foo       fedora      32
      
      $`fixtures/linux/fedora/33`
        raw distribution release
      1 foo       fedora      33
      
      $`fixtures/linux/fedora/34`
        raw distribution release
      1 foo       fedora      34
      
      $`fixtures/linux/fedora/35`
        raw distribution release
      1 foo       fedora      35
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/opensuse/15.0`
        raw  distribution release
      1 foo opensuse-leap    15.0
      
      $`fixtures/linux/opensuse/15.1`
        raw  distribution release
      1 foo opensuse-leap    15.1
      
      $`fixtures/linux/opensuse/15.2`
        raw  distribution release
      1 foo opensuse-leap    15.2
      
      $`fixtures/linux/opensuse/15.3`
        raw  distribution release
      1 foo opensuse-leap    15.3
      
      $`fixtures/linux/opensuse/42.3`
        raw distribution release
      1 foo     opensuse    42.3
      
      $`fixtures/linux/opensuse/tumbleweed`
        raw        distribution  release
      1 foo opensuse-tumbleweed 20210810
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/oraclelinux/6`
        raw distribution release
      1 foo           ol    6.10
      
      $`fixtures/linux/oraclelinux/7`
        raw distribution release
      1 foo           ol     7.9
      
      $`fixtures/linux/oraclelinux/8`
        raw distribution release
      1 foo           ol     8.4
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/sl/6`
        raw distribution release
      1 foo   scientific    6.10
      
      $`fixtures/linux/sl/7`
        raw distribution release
      1 foo   scientific     7.9
      

---

    Code
      nlapply(etc, current_r_platform_data_linux, raw = raw)
    Output
      $`fixtures/linux/ubuntu/14.04`
        raw distribution release
      1 foo       ubuntu   14.04
      
      $`fixtures/linux/ubuntu/16.04`
        raw distribution release
      1 foo       ubuntu   16.04
      
      $`fixtures/linux/ubuntu/18.04`
        raw distribution release
      1 foo       ubuntu   18.04
      
      $`fixtures/linux/ubuntu/20.04`
        raw distribution release
      1 foo       ubuntu   20.04
      
      $`fixtures/linux/ubuntu/21.04`
        raw distribution release
      1 foo       ubuntu   21.04
      
      $`fixtures/linux/ubuntu/21.10`
        raw distribution release
      1 foo       ubuntu   21.10
      

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
      [1] '3.14'
    Code
      bioc_version("4.0.0")
    Output
      [1] '3.12'
    Code
      bioc_version("3.6.0")
    Output
      [1] '3.10'

# bioc_version_map

    Code
      as.data.frame(bioc_version_map())
    Output
         bioc_version r_version bioc_status
      1           1.6       2.1 out-of-date
      2           1.7       2.2 out-of-date
      3           1.8       2.3 out-of-date
      4           1.9       2.4 out-of-date
      5           2.0       2.5 out-of-date
      6           2.1       2.6 out-of-date
      7           2.2       2.7 out-of-date
      8           2.3       2.8 out-of-date
      9           2.4       2.9 out-of-date
      10          2.5      2.10 out-of-date
      11          2.6      2.11 out-of-date
      12          2.7      2.12 out-of-date
      13          2.8      2.13 out-of-date
      14          2.9      2.14 out-of-date
      15         2.10      2.15 out-of-date
      16         2.11      2.15 out-of-date
      17         2.12       3.0 out-of-date
      18         2.13       3.0 out-of-date
      19         2.14       3.1 out-of-date
      20          3.0       3.1 out-of-date
      21          3.1       3.2 out-of-date
      22          3.2       3.2 out-of-date
      23          3.3       3.3 out-of-date
      24          3.4       3.3 out-of-date
      25          3.5       3.4 out-of-date
      26          3.6       3.4 out-of-date
      27          3.7       3.5 out-of-date
      28          3.8       3.5 out-of-date
      29          3.9       3.6 out-of-date
      30         3.10       3.6 out-of-date
      31         3.11       4.0 out-of-date
      32         3.12       4.0 out-of-date
      33         3.13       4.1 out-of-date
      34         3.14       4.1 out-of-date
      35         3.15       4.2     release
      36         3.16       4.2       devel
      37         3.16       4.3      future

# bioc_release_version, bioc_devel_version

    Code
      bioc_release_version()
    Output
      [1] '3.15'

---

    Code
      bioc_devel_version()
    Output
      [1] '3.16'

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

