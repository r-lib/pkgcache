# parse_description

    Code
      .Call(pkgcache_parse_description_raw, charToRaw(d))
    Condition
      Error:
      ! Invalid DESCRIPTION file, must start with an alphanumeric character @lib.c:<linum> (pkgcache_parse_description_raw)

---

    Code
      .Call(pkgcache_parse_description_raw, charToRaw(d))
    Condition
      Error:
      ! Line 1 invalid in DESCRIPTION: must be of form `key: value` @lib.c:<linum> (pkgcache_parse_description_raw)

---

    Code
      .Call(pkgcache_parse_description_raw, charToRaw(d))
    Condition
      Error:
      ! DESCRIPTION file ended while parsing a key @lib.c:278 (pkgcache_parse_description_raw)

---

    Code
      .Call(pkgcache_parse_description_raw, charToRaw(d))
    Condition
      Error:
      ! Line 2 invalid in DESCRIPTION: must be of form `key: value` @lib.c:<linum> (pkgcache_parse_description_raw)

# parse_packages

    Code
      colnames(pkgs)
    Output
       [1] "Package"          "Version"         
       [3] "Depends"          "Suggests"        
       [5] "License"          "MD5sum"          
       [7] "NeedsCompilation" "Imports"         
       [9] "Priority"         "Path"            

---

    Code
      pkgs$Path
    Output
      [1] NA                  NA                 
      [3] NA                  "4.2.0/Recommended"
      [5] "Older"             "Older"            

# parse_packages, RDS

    Code
      colnames(pkgs)
    Output
       [1] "Package"               "Version"              
       [3] "Priority"              "Depends"              
       [5] "Imports"               "LinkingTo"            
       [7] "Suggests"              "Enhances"             
       [9] "License"               "License_is_FOSS"      
      [11] "License_restricts_use" "OS_type"              
      [13] "Archs"                 "MD5sum"               
      [15] "NeedsCompilation"      "Path"                 

---

    Code
      pkgs$Path
    Output
      [1] NA                  NA                 
      [3] NA                  "4.2.0/Recommended"
      [5] "Older"             "Older"            

---

    Code
      colnames(pkgs)
    Output
       [1] "Package"               "Version"              
       [3] "Priority"              "Depends"              
       [5] "Imports"               "LinkingTo"            
       [7] "Suggests"              "Enhances"             
       [9] "License"               "License_is_FOSS"      
      [11] "License_restricts_use" "OS_type"              
      [13] "Archs"                 "MD5sum"               
      [15] "NeedsCompilation"      "Path"                 

---

    Code
      pkgs$Path
    Output
      [1] NA                  NA                 
      [3] NA                  "4.2.0/Recommended"
      [5] "Older"             "Older"            

# parse_packages, errors

    Code
      .Call(pkgcache_parse_packages_raw, charToRaw(p))
    Condition
      Error:
      ! Invalid PACKAGES file in line 3: expected key @lib.c:<linum> (pkgcache_parse_packages_raw)

---

    Code
      .Call(pkgcache_parse_packages_raw, charToRaw(p))
    Condition
      Error:
      ! Invalid line 2 in PACKAGES file: must contain `:` @lib.c:<linum> (pkgcache_parse_packages_raw)

---

    Code
      .Call(pkgcache_parse_packages_raw, charToRaw(p))
    Condition
      Error:
      ! PACKAGES file ended while parsing a key @lib.c:487 (pkgcache_parse_packages_raw)

# somewhat weird packages files

    Code
      colnames(pkgs)
    Output
      [1] "Package"          "Version"          "Depends"         
      [4] "Suggests"         "License"          "MD5sum"          
      [7] "NeedsCompilation" "Imports"         

---

    Code
      pkgs$Package
    Output
      [1] "A3"    "aaSEA"

# parse_packages empty file

    Code
      parse_packages(tmp)
    Output
      # A data frame: 0 x 0

# parse_installed

    Code
      pkgs$Package
    Output
      [1] "cli"   "rlang"

---

    Code
      pkgs2$Package
    Output
      [1] "cli"

# parse_installed, DESCRIPTION with <CR><LF>

    Code
      pkgs$Package
    Output
      [1] "cli"   "rlang"

# parse_installed, multiple libs

    Code
      pkgs$Package
    Output
      [1] "cli"   "rlang" "pak"  

# parse_installed, errors

    Code
      pkgs$Package
    Output
      [1] "cli"      "rlang"    "pak"      "pkgcache"

# parse_installed priority

    Code
      parse_installed(lib5, priority = "base")$Package
    Output
      [1] "stats"

---

    Code
      parse_installed(lib5, priority = "recommended")$Package
    Output
      [1] "Matrix"

---

    Code
      parse_installed(lib5, priority = NA)$Package
    Output
      [1] "cli"   "rlang"

---

    Code
      parse_installed(lib5, priority = c("base", "recommended"))$
        Package
    Output
      [1] "Matrix" "stats" 

---

    Code
      parse_installed(lib5, priority = c("base", NA))$Package
    Output
      [1] "cli"   "rlang" "stats"

# fix_encodings

    Code
      lst2$Package
    Output
      [1] "foo"    "bar"    "foobar"

---

    Code
      lst2$Encoding
    Output
      [1] NA       "UTF-8"  "latin1"

---

    Code
      Encoding(lst2$Maintainer)
    Output
      [1] "unknown" "UTF-8"   "UTF-8"  

---

    Code
      lapply(lst2$Maintainer, charToRaw)
    Output
      [[1]]
      [1] 47 61 62 6f 72
      
      [[2]]
      [1] 47 c3 a1 62 6f 72
      
      [[3]]
      [1] 47 c3 a1 62 6f 72
      

---

    Code
      Encoding(lst2$Bad)
    Output
      [1] "bytes"   "UTF-8"   "unknown"

---

    Code
      lapply(lst2$Bad, charToRaw)
    Output
      [[1]]
      [1] 47 c3 a1 62 6f 72
      
      [[2]]
      [1] 47 e1 62 6f 72
      
      [[3]]
      [1] 47 61 62 6f 72
      

# fix encodings on data frames

    Code
      tbl2$Package
    Output
      [1] "foo"    "bar"    "foobar"

---

    Code
      tbl2$Encoding
    Output
      [1] NA       "UTF-8"  "latin1"

---

    Code
      Encoding(tbl2$Maintainer)
    Output
      [1] "unknown" "UTF-8"   "UTF-8"  

---

    Code
      lapply(tbl2$Maintainer, charToRaw)
    Output
      [[1]]
      [1] 47 61 62 6f 72
      
      [[2]]
      [1] 47 c3 a1 62 6f 72
      
      [[3]]
      [1] 47 c3 a1 62 6f 72
      

---

    Code
      Encoding(tbl2$Bad)
    Output
      [1] "bytes"   "UTF-8"   "unknown"

---

    Code
      lapply(tbl2$Bad, charToRaw)
    Output
      [[1]]
      [1] 47 c3 a1 62 6f 72
      
      [[2]]
      [1] 47 e1 62 6f 72
      
      [[3]]
      [1] 47 61 62 6f 72
      

