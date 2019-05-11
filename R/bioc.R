
type_bioc_get_bioc_repos <- function(r_version) {
  bv <- type_bioc_matching_bioc_version(r_version)
  tmpl <- c(
    BioCsoft  = "https://bioconductor.org/packages/{bv}/bioc",
    BioCann   = "https://bioconductor.org/packages/{bv}/data/annotation",
    BioCexp   = "https://bioconductor.org/packages/{bv}/data/experiment",
    BioCextra = if (package_version(bv) <= 3.5) {
                  "https://bioconductor.org/packages/{bv}/extra"
                }
  )
  list(
    repos = sub("{bv}", bv, tmpl, fixed = TRUE),
    version = bv
  )
}

type_bioc_matching_bioc_version <- function(r_version) {
  if (r_version >= "3.5") {
    "3.7"
  } else if (r_version >= "3.4") {
    "3.6"
  } else if (r_version >= "3.3.0") {
    "3.4"
  } else if (r_version >= "3.2") {
    "3.2"
  } else if (r_version >= "3.1.1") {
    "3.0"
  } else if (r_version == "3.1.0") {
    "2.14"
  } else if (r_version >= "2.15" && r_version <= "2.16") {
    "2.11"
  } else {
    stop("Cannot get matching BioConductor version for ", r_version)
  }
}
