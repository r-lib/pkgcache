# read_packages_file windows

    Code
      print(pl)
    Output
      [1] "x86_64-w64-mingw32"
    Code
      structure(pkgs$pkgs$platform, names = pkgs$pkgs$package)
    Output
      $noarch
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $both
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $only64
      [1] "x86_64-w64-mingw32"
      

---

    Code
      print(pl)
    Output
      [1] "i386-w64-mingw32"
    Code
      structure(pkgs$pkgs$platform, names = pkgs$pkgs$package)
    Output
      $noarch
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $both
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $only32
      [1] "i386-w64-mingw32"
      

---

    Code
      print(pl)
    Output
      [1] "i386+x86_64-w64-mingw32"
    Code
      structure(pkgs$pkgs$platform, names = pkgs$pkgs$package)
    Output
      $noarch
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $both
      [1] "x86_64-w64-mingw32" "i386-w64-mingw32"  
      
      $only64
      [1] "x86_64-w64-mingw32"
      
      $only32
      [1] "i386-w64-mingw32"
      

