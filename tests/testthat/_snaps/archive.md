# API

    Code
      cran_archive_list()
    Output
      # A data frame: 3 x 6
        package version raw               mtime               url                                                           mirror                 
      * <chr>   <chr>   <chr>             <dttm>              <chr>                                                         <chr>                  
      1 pkg1    0.9.0   pkg1_0.9.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.9.0.tar.gz http://127.0.0.1:3000/
      2 pkg1    0.8.0   pkg1_0.8.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.8.0.tar.gz http://127.0.0.1:3000/
      3 pkg3    0.9.9   pkg3_0.9.9.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg3_0.9.9.tar.gz http://127.0.0.1:3000/

---

    Code
      cran_archive_update()
      cran_archive_list()
    Output
      # A data frame: 3 x 6
        package version raw               mtime               url                                                           mirror                 
      * <chr>   <chr>   <chr>             <dttm>              <chr>                                                         <chr>                  
      1 pkg1    0.9.0   pkg1_0.9.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.9.0.tar.gz http://127.0.0.1:3000/
      2 pkg1    0.8.0   pkg1_0.8.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.8.0.tar.gz http://127.0.0.1:3000/
      3 pkg3    0.9.9   pkg3_0.9.9.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg3_0.9.9.tar.gz http://127.0.0.1:3000/

---

    Code
      cran_archive_list(packages = "pkg1")
    Output
      # A data frame: 2 x 6
        package version raw               mtime               url                                                           mirror                 
      * <chr>   <chr>   <chr>             <dttm>              <chr>                                                         <chr>                  
      1 pkg1    0.9.0   pkg1_0.9.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.9.0.tar.gz http://127.0.0.1:3000/
      2 pkg1    0.8.0   pkg1_0.8.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.8.0.tar.gz http://127.0.0.1:3000/

# cran_archive_cache

    Code
      cac$check_update()
    Output
      # A data frame: 3 x 6
        package version raw               mtime               url                                                           mirror                 
      * <chr>   <chr>   <chr>             <dttm>              <chr>                                                         <chr>                  
      1 pkg1    0.9.0   pkg1_0.9.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.9.0.tar.gz http://127.0.0.1:3000/
      2 pkg1    0.8.0   pkg1_0.8.0.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg1_0.8.0.tar.gz http://127.0.0.1:3000/
      3 pkg3    0.9.9   pkg3_0.9.9.tar.gz 2022-09-14 13:3000:3000 http://127.0.0.1:3000//src/contrib/Archive/pkg3_0.9.9.tar.gz http://127.0.0.1:3000/

