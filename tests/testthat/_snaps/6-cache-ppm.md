# add binary

    Code
      pkgs[, cols]
    Output
                                                                     path package url
      1 src/contrib/x86_64-pc-linux-gnu-ubuntu-22.04/4.2/pkg_1.0.0.tar.gz      NA  NA
        etag                                                           sha256
      1   NA 32f7b637cf5ab87ba96679679e673901c339626b513a6b9d033d360302602080
                                platform
      1 x86_64-pc-linux-gnu-ubuntu-22.04

# add source

    Code
      pkgs[, cols]
    Output
                                path package url etag
      1 src/contrib/pkg_1.0.0.tar.gz      NA  NA   NA
                                                                  sha256 platform
      1 32f7b637cf5ab87ba96679679e673901c339626b513a6b9d033d360302602080   source

# binary expected, got source

    Code
      pkgs[, cols]
    Output
                                path package url etag
      1 src/contrib/pkg_1.0.0.tar.gz      NA  NA   NA
                                                                  sha256 platform
      1 32f7b637cf5ab87ba96679679e673901c339626b513a6b9d033d360302602080   source

# source expected, got binary

    Code
      pkgs[, cols]
    Output
                                                   path package url etag
      1 src/contrib/*-ubuntu-22.04/4.2/pkg_1.0.0.tar.gz      NA  NA   NA
                                                                  sha256
      1 32f7b637cf5ab87ba96679679e673901c339626b513a6b9d033d360302602080
              platform
      1 *-ubuntu-22.04

