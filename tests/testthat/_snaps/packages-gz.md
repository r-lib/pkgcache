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
        package  platform               rversion
        <chr>    <chr>                  <chr>   
      1 filelock source                 *       
      2 fs       aarch64-apple-darwin20 4.2     
      3 ps       aarch64-apple-darwin20 4.2     
      4 zip      source                 *       

---

    Code
      pkgs$pkgs$target
    Output
      [1] "src/contrib/filelock_1.0.2.tar.gz"                     
      [2] "src/contrib/<platform>/4.2/fs_1.6.1.tar.gz"
      [3] "src/contrib/<platform>/4.2/ps_1.7.2.tar.gz"
      [4] "src/contrib/zip_2.2.2.tar.gz"                          

