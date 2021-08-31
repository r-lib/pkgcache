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

# lib_status

    Code
      pkgs$Package
    Output
      [1] "cli"   "rlang"

# lib_status, multiple libs

    Code
      pkgs$Package
    Output
      [1] "cli"   "rlang" "pak"  

# lib_status, errors

    Code
      pkgs$Package
    Output
      [1] "cli"      "rlang"    "pak"      "pkgcache"

