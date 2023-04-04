# read_packages_file windows

    Code
      print(pl)
    Output
      [1] "x86_64-w64-mingw32"
    Code
      pkgs$pkgs[, c("package", "platform")]
    Output
      # A data frame: 3 x 2
        package platform               
      * <chr>   <chr>                  
      1 noarch  i386+x86_64-w64-mingw32
      2 both    i386+x86_64-w64-mingw32
      3 only64  x86_64-w64-mingw32     

---

    Code
      print(pl)
    Output
      [1] "i386-w64-mingw32"
    Code
      pkgs$pkgs[, c("package", "platform")]
    Output
      # A data frame: 3 x 2
        package platform               
      * <chr>   <chr>                  
      1 noarch  i386+x86_64-w64-mingw32
      2 both    i386+x86_64-w64-mingw32
      3 only32  i386-w64-mingw32       

---

    Code
      print(pl)
    Output
      [1] "i386+x86_64-w64-mingw32"
    Code
      pkgs$pkgs[, c("package", "platform")]
    Output
      # A data frame: 4 x 2
        package platform               
        <chr>   <chr>                  
      1 noarch  i386+x86_64-w64-mingw32
      2 both    i386+x86_64-w64-mingw32
      3 only64  x86_64-w64-mingw32     
      4 only32  i386-w64-mingw32       

# read_packages_file from PPM

    Code
      pkgs$pkgs[, cols]
    Output
      # A data frame: 4 x 3
        package  platform           rversion
        <chr>    <chr>              <chr>   
      1 filelock source             *       
      2 fs       <current-platform> 4.2     
      3 ps       <current-platform> 4.2     
      4 zip      source             *       

---

    Code
      pkgs$pkgs$target
    Output
      [1] "src/contrib/filelock_1.0.2.tar.gz"                 
      [2] "src/contrib/<current-platform>/4.2/fs_1.6.1.tar.gz"
      [3] "src/contrib/<current-platform>/4.2/ps_1.7.2.tar.gz"
      [4] "src/contrib/zip_2.2.2.tar.gz"                      

# rversion and platform

    Code
      as.list(pkgs$pkgs[, c("package", "target", "sources", "rversion", "platform")])
    Output
      $package
      [1] "cli"      "filelock"
      
      $target
      [1] "src/contrib/cli_3.6.1_R4.4_x86_64-pc-linux-gnu-ubuntu-22.04.tar.gz"     
      [2] "src/contrib/filelock_1.0.2_R4.4_x86_64-pc-linux-gnu-ubuntu-22.04.tar.gz"
      
      $sources
      $sources[[1]]
      [1] "https://github.com/cran/cli/releases/download/3.6.1/cli_3.6.1_R4.4_x86_64-pc-linux-gnu-ubuntu-22.04.tar.gz"
      
      $sources[[2]]
      [1] "https://github.com/cran/filelock/releases/download/1.0.2/filelock_1.0.2_R4.4_x86_64-pc-linux-gnu-ubuntu-22.04.tar.gz"
      
      
      $rversion
      [1] "4.4" "4.4"
      
      $platform
      [1] "x86_64-pc-linux-gnu-ubuntu-22.04" "x86_64-pc-linux-gnu-ubuntu-22.04"
      

