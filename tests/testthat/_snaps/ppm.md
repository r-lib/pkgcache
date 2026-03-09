# ppm_snapshots

    Code
      ppm_snapshots()[1:1000, ]
    Output
      # A data frame: 1,000 x 2
         date       id        
       * <date>     <chr>     
       1 2017-10-10 2017-10-10
       2 2017-10-11 2017-10-11
       3 2017-10-12 2017-10-12
       4 2017-10-13 2017-10-13
       5 2017-10-14 2017-10-14
       6 2017-10-15 2017-10-15
       7 2017-10-16 2017-10-16
       8 2017-10-17 2017-10-17
       9 2017-10-18 2017-10-18
      10 2017-10-19 2017-10-19
      # i 990 more rows

# ppm_platforms

    Code
      ppm_platforms()
    Output
      # A data frame: 3 x 6
        name    os    binary_url distribution release binaries
        <chr>   <chr> <chr>      <chr>        <chr>   <lgl>   
      1 centos7 linux centos7    centos       7       TRUE    
      2 centos8 linux centos8    centos       8       TRUE    
      3 rhel9   linux rhel9      rockylinux   9       TRUE    

# async_get_ppm_status

    Code
      synchronise(async_get_ppm_status(forget = TRUE))
    Condition
      Error in `download_file()`:
      ! nope

---

    Code
      synchronise(async_get_ppm_status(distribution = "123"))
    Condition
      Error in `download_file()`:
      ! nope

---

    Code
      synchronise(async_get_ppm_status(distribution = "ubuntu", release = "123"))
    Condition
      Error in `download_file()`:
      ! nope

# async_get_ppm_status 2

    Code
      ret
    Output
                          name      os     binary_url distribution release binaries
      1                centos7   linux        centos7       centos       7     TRUE
      2                centos8   linux        centos8       centos       8     TRUE
      3                  rhel9   linux          rhel9   rockylinux       9     TRUE
      4                 rhel10   linux         rhel10   rockylinux      10     TRUE
      5             opensuse15   linux     opensuse15     opensuse      15     TRUE
      6            opensuse152   linux    opensuse152     opensuse    15.2     TRUE
      7            opensuse153   linux    opensuse153     opensuse    15.3     TRUE
      8            opensuse154   linux    opensuse154     opensuse    15.4     TRUE
      9            opensuse155   linux    opensuse155     opensuse    15.5     TRUE
      10           opensuse156   linux    opensuse156     opensuse    15.6     TRUE
      11            opensuse42   linux     opensuse42     opensuse    42.3     TRUE
      12                 rhel7   linux        centos7       redhat       7     TRUE
      13                 rhel8   linux        centos8       redhat       8     TRUE
      14  rhel9 (unused alias)   linux          rhel9       redhat       9     TRUE
      15 rhel10 (unused alias)   linux         rhel10       redhat      10     TRUE
      16                sles12   linux     opensuse42          sle    12.3     TRUE
      17                sles15   linux     opensuse15          sle      15     TRUE
      18               sles152   linux    opensuse152          sle    15.2     TRUE
      19               sles153   linux    opensuse153          sle    15.3     TRUE
      20               sles154   linux    opensuse154          sle    15.4     TRUE
      21               sles155   linux    opensuse155          sle    15.5     TRUE
      22               sles156   linux    opensuse156          sle    15.6     TRUE
      23                xenial   linux         xenial       ubuntu   16.04     TRUE
      24                bionic   linux         bionic       ubuntu   18.04     TRUE
      25                 focal   linux          focal       ubuntu   20.04     TRUE
      26                 jammy   linux          jammy       ubuntu   22.04     TRUE
      27                 noble   linux          noble       ubuntu   24.04     TRUE
      28                buster   linux         buster       debian      10    FALSE
      29              bullseye   linux       bullseye       debian      11     TRUE
      30              bookworm   linux       bookworm       debian      12     TRUE
      31                trixie   linux         trixie       debian      13     TRUE
      32               windows windows                     windows     all     TRUE
      33                 macos   macos                       macos     all     TRUE
      34        manylinux_2_28   linux manylinux_2_28       centos       8     TRUE
      35              internal   linux       internal     internal     all     TRUE
                                                                                                                platforms
      1                                                                                                          centos-7
      2                                                                                                          centos-8
      3         rockylinux-9, rhel-9, /rhel-9[.][0-9]+/, rocky-9, /rocky-9[.][0-9]+/, almalinux-9, /almalinux-9[.][0-9]+/
      4  rockylinux-10, rhel-10, /rhel-10[.][0-9]+/, rocky-10, /rocky-10[.][0-9]+/, almalinux-10, /almalinux-10[.][0-9]+/
      5               opensuse-15, opensuse-15.0, opensuse-15.1, opensuse-leap-15, opensuse-leap-15.0, opensuse-leap-15.1
      6                                                                                 opensuse-15.2, opensuse-leap-15.2
      7                                                                                 opensuse-15.3, opensuse-leap-15.3
      8                                                                                 opensuse-15.4, opensuse-leap-15.4
      9                                                                                 opensuse-15.5, opensuse-leap-15.5
      10                                                                                opensuse-15.6, opensuse-leap-15.6
      11                                                                                opensuse-42.3, opensuse-leap-42.3
      12                                                                              redhat-7, rhel-7, /rhel-7[.][0-9]+/
      13            redhat-8, rhel-8, /rhel-8[.][0-9]+/, rocky-8, /rocky-8[.][0-9]+/, almalinux-8, /almalinux-8[.][0-9]+/
      14                                                                                                         redhat-9
      15                                                                                                        redhat-10
      16                                                                                              sle-12.3, sles-12.3
      17                                                                             sle-15, sle-15.1, sles-15, sles-15.1
      18                                                                                              sle-15.2, sles-15.2
      19                                                                                              sle-15.3, sles-15.3
      20                                                                                              sle-15.4, sles-15.4
      21                                                                                              sle-15.5, sles-15.5
      22                                                                                              sle-15.6, sles-15.6
      23                                                                                                     ubuntu-16.04
      24                                                                                                     ubuntu-18.04
      25                                                                                                     ubuntu-20.04
      26                                                                                                     ubuntu-22.04
      27                                                                                                     ubuntu-24.04
      28                                                                                                        debian-10
      29                                                                                                        debian-11
      30                                                                                                        debian-12
      31                                                                                                        debian-13
      32                                                                                                      windows-all
      33                                                                                                        macos-all
      34                                                                                                         centos-8
      35                                                                                                     internal-all

# async_get_ppm_status 3

    Code
      ret
    Output
      [1] "4.5" "4.4" "4.3" "4.2" "4.1" "3.6"

# ppm_r_versions

    Code
      ppm_r_versions()
    Output
      # A data frame: 3 x 1
        r_version
        <chr>    
      1 3.5      
      2 3.6      
      3 4.2      

# pkgenv$ppm_distros_cached is current

    Code
      current
    Output
                          name      os     binary_url distribution release binaries
      1                centos7   linux        centos7       centos       7     TRUE
      2                centos8   linux        centos8       centos       8     TRUE
      3                  rhel9   linux          rhel9   rockylinux       9     TRUE
      4                 rhel10   linux         rhel10   rockylinux      10     TRUE
      5             opensuse15   linux     opensuse15     opensuse      15     TRUE
      6            opensuse152   linux    opensuse152     opensuse    15.2     TRUE
      7            opensuse153   linux    opensuse153     opensuse    15.3     TRUE
      8            opensuse154   linux    opensuse154     opensuse    15.4     TRUE
      9            opensuse155   linux    opensuse155     opensuse    15.5     TRUE
      10           opensuse156   linux    opensuse156     opensuse    15.6     TRUE
      11            opensuse42   linux     opensuse42     opensuse    42.3     TRUE
      12                 rhel7   linux        centos7       redhat       7     TRUE
      13                 rhel8   linux        centos8       redhat       8     TRUE
      14  rhel9 (unused alias)   linux          rhel9       redhat       9     TRUE
      15 rhel10 (unused alias)   linux         rhel10       redhat      10     TRUE
      16                sles12   linux     opensuse42          sle    12.3     TRUE
      17                sles15   linux     opensuse15          sle      15     TRUE
      18               sles152   linux    opensuse152          sle    15.2     TRUE
      19               sles153   linux    opensuse153          sle    15.3     TRUE
      20               sles154   linux    opensuse154          sle    15.4     TRUE
      21               sles155   linux    opensuse155          sle    15.5     TRUE
      22               sles156   linux    opensuse156          sle    15.6     TRUE
      23                xenial   linux         xenial       ubuntu   16.04     TRUE
      24                bionic   linux         bionic       ubuntu   18.04     TRUE
      25                 focal   linux          focal       ubuntu   20.04     TRUE
      26                 jammy   linux          jammy       ubuntu   22.04     TRUE
      27                 noble   linux          noble       ubuntu   24.04     TRUE
      28                buster   linux         buster       debian      10    FALSE
      29              bullseye   linux       bullseye       debian      11     TRUE
      30              bookworm   linux       bookworm       debian      12     TRUE
      31                trixie   linux         trixie       debian      13     TRUE
      32               windows windows                     windows     all     TRUE
      33                 macos   macos                       macos     all     TRUE
      34        manylinux_2_28   linux manylinux_2_28       centos       8     TRUE
      35              internal   linux       internal     internal     all     TRUE
                                                                                                                platforms
      1                                                                                                          centos-7
      2                                                                                                          centos-8
      3         rockylinux-9, rhel-9, /rhel-9[.][0-9]+/, rocky-9, /rocky-9[.][0-9]+/, almalinux-9, /almalinux-9[.][0-9]+/
      4  rockylinux-10, rhel-10, /rhel-10[.][0-9]+/, rocky-10, /rocky-10[.][0-9]+/, almalinux-10, /almalinux-10[.][0-9]+/
      5               opensuse-15, opensuse-15.0, opensuse-15.1, opensuse-leap-15, opensuse-leap-15.0, opensuse-leap-15.1
      6                                                                                 opensuse-15.2, opensuse-leap-15.2
      7                                                                                 opensuse-15.3, opensuse-leap-15.3
      8                                                                                 opensuse-15.4, opensuse-leap-15.4
      9                                                                                 opensuse-15.5, opensuse-leap-15.5
      10                                                                                opensuse-15.6, opensuse-leap-15.6
      11                                                                                opensuse-42.3, opensuse-leap-42.3
      12                                                                              redhat-7, rhel-7, /rhel-7[.][0-9]+/
      13            redhat-8, rhel-8, /rhel-8[.][0-9]+/, rocky-8, /rocky-8[.][0-9]+/, almalinux-8, /almalinux-8[.][0-9]+/
      14                                                                                                         redhat-9
      15                                                                                                        redhat-10
      16                                                                                              sle-12.3, sles-12.3
      17                                                                             sle-15, sle-15.1, sles-15, sles-15.1
      18                                                                                              sle-15.2, sles-15.2
      19                                                                                              sle-15.3, sles-15.3
      20                                                                                              sle-15.4, sles-15.4
      21                                                                                              sle-15.5, sles-15.5
      22                                                                                              sle-15.6, sles-15.6
      23                                                                                                     ubuntu-16.04
      24                                                                                                     ubuntu-18.04
      25                                                                                                     ubuntu-20.04
      26                                                                                                     ubuntu-22.04
      27                                                                                                     ubuntu-24.04
      28                                                                                                        debian-10
      29                                                                                                        debian-11
      30                                                                                                        debian-12
      31                                                                                                        debian-13
      32                                                                                                      windows-all
      33                                                                                                        macos-all
      34                                                                                                         centos-8
      35                                                                                                     internal-all

