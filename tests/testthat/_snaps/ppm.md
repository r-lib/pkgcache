# ppm_snapshots

    Code
      ppm_snapshots()
    Output
      # A data frame: 6 x 2
        date                      id
        <dttm>                 <int>
      1 2023-02-20 00:00:00 16856763
      2 2023-02-21 00:00:00 16882269
      3 2023-02-22 00:00:00 16903694
      4 2023-02-24 00:00:00 16958782
      5 2023-02-27 00:00:00 17028146
      6 2023-02-28 00:00:00 17054670

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

# async_get_ppm_versions 2

    Code
      ret
    Output
      2021-01-25T00:00:00Z 2021-01-26T00:00:00Z 2021-01-27T00:00:00Z 
                  "997643"            "1014755"            "1033374" 
      2021-01-28T00:00:00Z 2021-01-29T00:00:00Z 2021-02-01T00:00:00Z 
                 "1053473"            "1069075"            "1123445" 
      2021-02-02T00:00:00Z 2021-02-03T00:00:00Z 2021-02-04T00:00:00Z 
                 "1140568"            "1160641"            "1175516" 
      2021-02-05T00:00:00Z 
                 "1194160" 

# async_get_ppm_status 2

    Code
      ret
    Output
                         name      os  binary_url distribution release binaries
      1               centos7   linux     centos7       centos       7     TRUE
      2               centos8   linux     centos8       centos       8     TRUE
      3                 rhel9   linux       rhel9   rockylinux       9     TRUE
      4            opensuse15   linux  opensuse15     opensuse      15     TRUE
      5           opensuse152   linux opensuse152     opensuse    15.2     TRUE
      6           opensuse153   linux opensuse153     opensuse    15.3     TRUE
      7           opensuse154   linux opensuse154     opensuse    15.4     TRUE
      8            opensuse42   linux  opensuse42     opensuse    42.3     TRUE
      9                 rhel7   linux     centos7       redhat       7     TRUE
      10                rhel8   linux     centos8       redhat       8     TRUE
      11 rhel9 (unused alias)   linux       rhel9       redhat       9     TRUE
      12               sles12   linux  opensuse42          sle    12.3     TRUE
      13               sles15   linux  opensuse15          sle      15     TRUE
      14              sles152   linux opensuse152          sle    15.2     TRUE
      15              sles153   linux opensuse153          sle    15.3     TRUE
      16              sles154   linux opensuse154          sle    15.4     TRUE
      17               xenial   linux      xenial       ubuntu   16.04     TRUE
      18               bionic   linux      bionic       ubuntu   18.04     TRUE
      19                focal   linux       focal       ubuntu   20.04     TRUE
      20                jammy   linux       jammy       ubuntu   22.04     TRUE
      21               buster   linux      buster       debian      10    FALSE
      22             bullseye   linux    bullseye       debian      11    FALSE
      23              windows windows                  windows     all     TRUE
      24                macOS   macOS                    macOS     all    FALSE

# async_get_ppm_status 3

    Code
      ret
    Output
      [1] "3.5" "3.6" "4.0" "4.1" "4.2"

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

