platforms <- utils::read.table(
  header = TRUE,
  stringsAsFactors = FALSE,
  textConnection(
    "
id              version     image
almalinux       8           almalinux:8
almalinux       9           almalinux:9
alpine          3.18        alpine:3.18
alpine          3.19        alpine:3.19
alpine          3.20        alpine:3.20
alpine          3.21        alpine:3.21
alpine          edge        alpine:edge
arch            base        archlinux:base
arch            base-devel  archlinux:base-devel
centos          7           centos:7
centos          8           centos:8
debian          8           debian:8
debian          9           debian:9
debian          10          debian:10
debian          11          debian:11
debian          12          debian:12
debian          testing     debian:testing
debian          unstable    debian:unstable
fedora          38          fedora:38
fedora          39          fedora:39
fedora          40          fedora:40
fedora          41          fedora:41
fedora          42          fedora:42
opensuse        15.3        opensuse/leap:15.3
opensuse        15.4        opensuse/leap:15.4
opensuse        15.5        opensuse/leap:15.5
opensuse        15.6        opensuse/leap:15.6
ubuntu          16.04       ubuntu:16.04
ubuntu          18.04       ubuntu:18.04
ubuntu          20.04       ubuntu:20.04
ubuntu          22.04       ubuntu:22.04
ubuntu          22.10       ubuntu:22.10
ubuntu          24.04       ubuntu:24.04
rhel            7           registry.access.redhat.com/ubi7/ubi
rhel            8           redhat/ubi8
rhel            9           redhat/ubi9
sles            15.3        registry.suse.com/bci/bci-base:15.3
sles            15.4        registry.suse.com/bci/bci-base:15.4
sles            15.5        registry.suse.com/bci/bci-base:15.5
sles            15.6        registry.suse.com/bci/bci-base:15.6
rocky           8           rockylinux:8
rocky           9           rockylinux:9
"
  )
)

for (idx in seq_len(nrow(platforms))) {
  tgtdir <- paste0("linux/", platforms$id[idx], "/", platforms$version[idx])
  dir.create(tgtdir, recursive = TRUE, showWarnings = FALSE)
  processx::run(
    echo_cmd = TRUE,
    "docker",
    c(
      "run",
      "--rm",
      "--platform",
      "linux/amd64",
      "-v",
      paste0("./", tgtdir, ":/output"),
      platforms$image[idx],
      "sh",
      "-c",
      "cp /etc/os-release /output"
    )
  )
}
