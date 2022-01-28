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

