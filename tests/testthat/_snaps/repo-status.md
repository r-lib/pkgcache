# repo_status

    Code
      print(cf)
    Output
      [[1]]
      [1] "source"
      
      [[2]]
      [1] "4.2.1"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 1 x 10
        name  url                     type  bioc_version platform path        r_version ok     ping error 
        <chr> <chr>                   <chr> <chr>        <chr>    <chr>       <chr>     <lgl> <dbl> <list>
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source   src/contrib 4.2       TRUE    0.1 <NULL>
    Code
      summary(stat)
    Output
      Repository summary:      source          
      CRAN @ 127.0.0.1:3000     OK     (100ms)

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "aarch64-apple-darwin20" "source"                
      
      [[2]]
      [1] "4.2.1"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform               path                                 r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                  <chr>                                <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                 src/contrib                          4.2       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         aarch64-apple-darwin20 bin/macosx/big-sur-arm64/contrib/4.2 4.2       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source aarch64-apple-darwin20       
      CRAN @ 127.0.0.1:3000     OK             --               

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "aarch64-apple-darwin20" "source"                
      
      [[2]]
      [1] "4.1.2" "4.2.1"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 4 x 10
        name  url                     type  bioc_version platform               path                                 r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                  <chr>                                <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                 src/contrib                          4.1       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                 src/contrib                          4.2       TRUE    0.1 <NULL>    
      3 CRAN  http://127.0.0.1:3000/ cran  <NA>         aarch64-apple-darwin20 bin/macosx/big-sur-arm64/contrib/4.1 4.1       FALSE  NA   <async_rj>
      4 CRAN  http://127.0.0.1:3000/ cran  <NA>         aarch64-apple-darwin20 bin/macosx/big-sur-arm64/contrib/4.2 4.2       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:              source aarch64-apple-darwin20       
      CRAN @ 127.0.0.1:3000 (R 4.1)     OK             --               
      CRAN @ 127.0.0.1:3000 (R 4.2)     OK             --               

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "x86_64-apple-darwin17.0" "source"                 
      
      [[2]]
      [1] "4.2.1"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform                path                   r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                   <chr>                  <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                  src/contrib            4.2       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         x86_64-apple-darwin17.0 bin/macosx/contrib/4.2 4.2       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source x86_64-apple-darwin17.0       
      CRAN @ 127.0.0.1:3000     OK             --                

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "windows" "source" 
      
      [[2]]
      [1] "4.2.1"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform                path                    r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                   <chr>                   <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                  src/contrib             4.2       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         i386+x86_64-w64-mingw32 bin/windows/contrib/4.2 4.2       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source i386+x86_64-w64-mingw32       
      CRAN @ 127.0.0.1:3000     OK             --                

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "windows" "source" 
      
      [[2]]
      [1] "4.0.5"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform                path                    r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                   <chr>                   <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                  src/contrib             4.0       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         i386+x86_64-w64-mingw32 bin/windows/contrib/4.0 4.0       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source i386+x86_64-w64-mingw32       
      CRAN @ 127.0.0.1:3000     OK             --                

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "windows" "source" 
      
      [[2]]
      [1] "3.6.3"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform                path                    r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                   <chr>                   <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                  src/contrib             3.6       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         i386+x86_64-w64-mingw32 bin/windows/contrib/3.6 3.6       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source i386+x86_64-w64-mingw32       
      CRAN @ 127.0.0.1:3000     OK             --                

---

    Code
      print(cf)
    Output
      [[1]]
      [1] "windows" "source" 
      
      [[2]]
      [1] "3.5.3"
      
    Code
      stat <- repo_status(bioc = FALSE, platforms = cf[[1]], r_version = cf[[2]])
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform                path                    r_version ok     ping error     
        <chr> <chr>                   <chr> <chr>        <chr>                   <chr>                   <chr>     <lgl> <dbl> <list>    
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                  src/contrib             3.5       TRUE    0.1 <NULL>    
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         i386+x86_64-w64-mingw32 bin/windows/contrib/3.5 3.5       FALSE  NA   <async_rj>
    Code
      summary(stat)
    Output
      Repository summary:      source i386+x86_64-w64-mingw32       
      CRAN @ 127.0.0.1:3000     OK             --                

# bioc repo status

    Code
      stat <- repo_status(bioc = TRUE, platforms = "source", r_version = "4.2")
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 5 x 10
        name          url                                                   type  bioc_version platform path        r_version ok     ping error 
        <chr>         <chr>                                                 <chr> <chr>        <chr>    <chr>       <chr>     <lgl> <dbl> <list>
      1 CRAN          http://127.0.0.1:3000/                               cran  <NA>         source   src/contrib 4.2       TRUE    0.1 <NULL>
      2 BioCsoft      http://127.0.0.1:3000//packages/3.16/bioc            bioc  3.16         source   src/contrib 4.2       TRUE    0.1 <NULL>
      3 BioCann       http://127.0.0.1:3000//packages/3.16/data/annotation bioc  3.16         source   src/contrib 4.2       TRUE    0.1 <NULL>
      4 BioCexp       http://127.0.0.1:3000//packages/3.16/data/experiment bioc  3.16         source   src/contrib 4.2       TRUE    0.1 <NULL>
      5 BioCworkflows http://127.0.0.1:3000//packages/3.16/workflows       bioc  3.16         source   src/contrib 4.2       TRUE    0.1 <NULL>
    Code
      summary(stat)
    Output
      Repository summary:               source          
      CRAN          @ 127.0.0.1:3000     OK     (100ms)
      BioCsoft      @ 127.0.0.1:3000     OK     (100ms)
      BioCann       @ 127.0.0.1:3000     OK     (100ms)
      BioCexp       @ 127.0.0.1:3000     OK     (100ms)
      BioCworkflows @ 127.0.0.1:3000     OK     (100ms)

# repo with binary packages

    Code
      stat <- repo_status(platforms = platforms, r_version = "4.2", bioc = FALSE)
      stat$ping[stat$ok] <- 0.1
      stat
    Output
      # A data frame: 2 x 10
        name  url                     type  bioc_version platform               path                                 r_version ok     ping error 
        <chr> <chr>                   <chr> <chr>        <chr>                  <chr>                                <chr>     <lgl> <dbl> <list>
      1 CRAN  http://127.0.0.1:3000/ cran  <NA>         source                 src/contrib                          4.2       TRUE    0.1 <NULL>
      2 CRAN  http://127.0.0.1:3000/ cran  <NA>         aarch64-apple-darwin20 bin/macosx/big-sur-arm64/contrib/4.2 4.2       TRUE    0.1 <NULL>
    Code
      summary(stat)
    Output
      Repository summary:      source aarch64-apple-darwin20          
      CRAN @ 127.0.0.1:3000     OK             OK             (100ms)

# repo_status unicode output [fancy]

    Code
      stat <- repo_status(bioc = FALSE, platforms = "source")
      stat$ping[stat$ok] <- 0.1
      summary(stat)
    Output
      Repository summary:      source[36m          [39m
      CRAN @ 127.0.0.1:3000   [32m  ✔   [39m[36m   (100ms)[39m

# convert repo_status summary to data frame

    Code
      stat <- repo_status(bioc = FALSE, platforms = "source")
      stat$ping[stat$ok] <- 0.1
      summary(stat)[]
    Output
      # A data frame: 1 x 3
        repository             source  ping
        <chr>                  <lgl>  <dbl>
      1 CRAN @ 127.0.0.1:3000 TRUE     0.1

