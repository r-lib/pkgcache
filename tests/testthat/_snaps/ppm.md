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
      1               centos7   linux        centos7       centos       7     TRUE
      2               centos8   linux        centos8       centos       8     TRUE
      3                 rhel9   linux          rhel9   rockylinux       9     TRUE
      4            opensuse15   linux     opensuse15     opensuse      15     TRUE
      5           opensuse152   linux    opensuse152     opensuse    15.2     TRUE
      6           opensuse153   linux    opensuse153     opensuse    15.3     TRUE
      7           opensuse154   linux    opensuse154     opensuse    15.4     TRUE
      8           opensuse155   linux    opensuse155     opensuse    15.5     TRUE
      9           opensuse156   linux    opensuse156     opensuse    15.6     TRUE
      10           opensuse42   linux     opensuse42     opensuse    42.3     TRUE
      11                rhel7   linux        centos7       redhat       7     TRUE
      12                rhel8   linux        centos8       redhat       8     TRUE
      13 rhel9 (unused alias)   linux          rhel9       redhat       9     TRUE
      14               sles12   linux     opensuse42          sle    12.3     TRUE
      15               sles15   linux     opensuse15          sle      15     TRUE
      16              sles152   linux    opensuse152          sle    15.2     TRUE
      17              sles153   linux    opensuse153          sle    15.3     TRUE
      18              sles154   linux    opensuse154          sle    15.4     TRUE
      19              sles155   linux    opensuse155          sle    15.5     TRUE
      20              sles156   linux    opensuse156          sle    15.6     TRUE
      21               xenial   linux         xenial       ubuntu   16.04     TRUE
      22               bionic   linux         bionic       ubuntu   18.04     TRUE
      23                focal   linux          focal       ubuntu   20.04     TRUE
      24                jammy   linux          jammy       ubuntu   22.04     TRUE
      25                noble   linux          noble       ubuntu   24.04     TRUE
      26               buster   linux         buster       debian      10    FALSE
      27             bullseye   linux       bullseye       debian      11     TRUE
      28             bookworm   linux       bookworm       debian      12     TRUE
      29              windows windows                     windows     all     TRUE
      30                macos   macos                       macos     all     TRUE
      31       manylinux_2_28   linux manylinux_2_28       centos       8     TRUE
      32             internal   linux       internal     internal     all     TRUE
                                                                                                         platforms
      1                                                                                                   centos-7
      2                                                                                                   centos-8
      3  rockylinux-9, rhel-9, /rhel-9[.][0-9]+/, rocky-9, /rocky-9[.][0-9]+/, almalinux-9, /almalinux-9[.][0-9]+/
      4        opensuse-15, opensuse-15.0, opensuse-15.1, opensuse-leap-15, opensuse-leap-15.0, opensuse-leap-15.1
      5                                                                          opensuse-15.2, opensuse-leap-15.2
      6                                                                          opensuse-15.3, opensuse-leap-15.3
      7                                                                          opensuse-15.4, opensuse-leap-15.4
      8                                                                          opensuse-15.5, opensuse-leap-15.5
      9                                                                          opensuse-15.6, opensuse-leap-15.6
      10                                                                         opensuse-42.3, opensuse-leap-42.3
      11                                                                       redhat-7, rhel-7, /rhel-7[.][0-9]+/
      12     redhat-8, rhel-8, /rhel-8[.][0-9]+/, rocky-8, /rocky-8[.][0-9]+/, almalinux-8, /almalinux-8[.][0-9]+/
      13                                                                                                  redhat-9
      14                                                                                       sle-12.3, sles-12.3
      15                                                                      sle-15, sle-15.1, sles-15, sles-15.1
      16                                                                                       sle-15.2, sles-15.2
      17                                                                                       sle-15.3, sles-15.3
      18                                                                                       sle-15.4, sles-15.4
      19                                                                                       sle-15.5, sles-15.5
      20                                                                                       sle-15.6, sles-15.6
      21                                                                                              ubuntu-16.04
      22                                                                                              ubuntu-18.04
      23                                                                                              ubuntu-20.04
      24                                                                                              ubuntu-22.04
      25                                                                                              ubuntu-24.04
      26                                                                                                 debian-10
      27                                                                                                 debian-11
      28                                                                                                 debian-12
      29                                                                                               windows-all
      30                                                                                                 macos-all
      31                                                                                                  centos-8
      32                                                                                              internal-all

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
      1               centos7   linux        centos7       centos       7     TRUE
      2               centos8   linux        centos8       centos       8     TRUE
      3                 rhel9   linux          rhel9   rockylinux       9     TRUE
      4            opensuse15   linux     opensuse15     opensuse      15     TRUE
      5           opensuse152   linux    opensuse152     opensuse    15.2     TRUE
      6           opensuse153   linux    opensuse153     opensuse    15.3     TRUE
      7           opensuse154   linux    opensuse154     opensuse    15.4     TRUE
      8           opensuse155   linux    opensuse155     opensuse    15.5     TRUE
      9           opensuse156   linux    opensuse156     opensuse    15.6     TRUE
      10           opensuse42   linux     opensuse42     opensuse    42.3     TRUE
      11                rhel7   linux        centos7       redhat       7     TRUE
      12                rhel8   linux        centos8       redhat       8     TRUE
      13 rhel9 (unused alias)   linux          rhel9       redhat       9     TRUE
      14               sles12   linux     opensuse42          sle    12.3     TRUE
      15               sles15   linux     opensuse15          sle      15     TRUE
      16              sles152   linux    opensuse152          sle    15.2     TRUE
      17              sles153   linux    opensuse153          sle    15.3     TRUE
      18              sles154   linux    opensuse154          sle    15.4     TRUE
      19              sles155   linux    opensuse155          sle    15.5     TRUE
      20              sles156   linux    opensuse156          sle    15.6     TRUE
      21               xenial   linux         xenial       ubuntu   16.04     TRUE
      22               bionic   linux         bionic       ubuntu   18.04     TRUE
      23                focal   linux          focal       ubuntu   20.04     TRUE
      24                jammy   linux          jammy       ubuntu   22.04     TRUE
      25                noble   linux          noble       ubuntu   24.04     TRUE
      26               buster   linux         buster       debian      10    FALSE
      27             bullseye   linux       bullseye       debian      11     TRUE
      28             bookworm   linux       bookworm       debian      12     TRUE
      29              windows windows                     windows     all     TRUE
      30                macos   macos                       macos     all     TRUE
      31       manylinux_2_28   linux manylinux_2_28       centos       8     TRUE
      32             internal   linux       internal     internal     all     TRUE
                                                                                                         platforms
      1                                                                                                   centos-7
      2                                                                                                   centos-8
      3  rockylinux-9, rhel-9, /rhel-9[.][0-9]+/, rocky-9, /rocky-9[.][0-9]+/, almalinux-9, /almalinux-9[.][0-9]+/
      4        opensuse-15, opensuse-15.0, opensuse-15.1, opensuse-leap-15, opensuse-leap-15.0, opensuse-leap-15.1
      5                                                                          opensuse-15.2, opensuse-leap-15.2
      6                                                                          opensuse-15.3, opensuse-leap-15.3
      7                                                                          opensuse-15.4, opensuse-leap-15.4
      8                                                                          opensuse-15.5, opensuse-leap-15.5
      9                                                                          opensuse-15.6, opensuse-leap-15.6
      10                                                                         opensuse-42.3, opensuse-leap-42.3
      11                                                                       redhat-7, rhel-7, /rhel-7[.][0-9]+/
      12     redhat-8, rhel-8, /rhel-8[.][0-9]+/, rocky-8, /rocky-8[.][0-9]+/, almalinux-8, /almalinux-8[.][0-9]+/
      13                                                                                                  redhat-9
      14                                                                                       sle-12.3, sles-12.3
      15                                                                      sle-15, sle-15.1, sles-15, sles-15.1
      16                                                                                       sle-15.2, sles-15.2
      17                                                                                       sle-15.3, sles-15.3
      18                                                                                       sle-15.4, sles-15.4
      19                                                                                       sle-15.5, sles-15.5
      20                                                                                       sle-15.6, sles-15.6
      21                                                                                              ubuntu-16.04
      22                                                                                              ubuntu-18.04
      23                                                                                              ubuntu-20.04
      24                                                                                              ubuntu-22.04
      25                                                                                              ubuntu-24.04
      26                                                                                                 debian-10
      27                                                                                                 debian-11
      28                                                                                                 debian-12
      29                                                                                               windows-all
      30                                                                                                 macos-all
      31                                                                                                  centos-8
      32                                                                                              internal-all

