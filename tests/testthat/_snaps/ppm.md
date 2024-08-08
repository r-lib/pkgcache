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

