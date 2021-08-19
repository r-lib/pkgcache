# current_r_platform_linux

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/almalinux/8 
             "foo-almalinux-8.4" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
             fixtures/linux/alpine/3.11        fixtures/linux/alpine/3.12 
                   "foo-alpine-3.11.11"               "foo-alpine-3.12.7" 
             fixtures/linux/alpine/3.13        fixtures/linux/alpine/3.14 
                    "foo-alpine-3.13.5"               "foo-alpine-3.14.1" 
             fixtures/linux/alpine/edge 
      "foo-alpine-3.15.0_alpha20210804" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
            fixtures/linux/arch/base fixtures/linux/arch/base-devel 
                          "foo-arch"                     "foo-arch" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/centos/5 fixtures/linux/centos/6 fixtures/linux/centos/7 
            "foo-centos-5.11"       "foo-centos-6.10"          "foo-centos-7" 
      fixtures/linux/centos/8 
               "foo-centos-8" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
           fixtures/linux/debian/10      fixtures/linux/debian/11 
                    "foo-debian-10"               "foo-debian-11" 
            fixtures/linux/debian/8       fixtures/linux/debian/9 
                     "foo-debian-8"                "foo-debian-9" 
      fixtures/linux/debian/testing 
                    "foo-debian-11" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/fedora/32 fixtures/linux/fedora/33 fixtures/linux/fedora/34 
               "foo-fedora-32"          "foo-fedora-33"          "foo-fedora-34" 
      fixtures/linux/fedora/35 
               "foo-fedora-35" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
            fixtures/linux/opensuse/15.0       fixtures/linux/opensuse/15.1 
                "foo-opensuse-leap-15.0"           "foo-opensuse-leap-15.1" 
            fixtures/linux/opensuse/15.2       fixtures/linux/opensuse/15.3 
                "foo-opensuse-leap-15.2"           "foo-opensuse-leap-15.3" 
            fixtures/linux/opensuse/42.3 fixtures/linux/opensuse/tumbleweed 
                     "foo-opensuse-42.3" "foo-opensuse-tumbleweed-20210810" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/oraclelinux/6 fixtures/linux/oraclelinux/7 
                     "foo-ol-6.10"                 "foo-ol-7.9" 
      fixtures/linux/oraclelinux/8 
                      "foo-ol-8.4" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
        fixtures/linux/sl/6   fixtures/linux/sl/7 
      "foo-scientific-6.10"  "foo-scientific-7.9" 

---

    Code
      vcapply(etc, current_r_platform_linux, raw = "foo")
    Output
      fixtures/linux/ubuntu/14.04 fixtures/linux/ubuntu/16.04 
               "foo-ubuntu-14.04"          "foo-ubuntu-16.04" 
      fixtures/linux/ubuntu/18.04 fixtures/linux/ubuntu/20.04 
               "foo-ubuntu-18.04"          "foo-ubuntu-20.04" 
      fixtures/linux/ubuntu/21.04 fixtures/linux/ubuntu/21.10 
               "foo-ubuntu-21.04"          "foo-ubuntu-21.10" 

