
bioconductor <- local({

  # -------------------------------------------------------------------
  # Configuration that does not change often

  config_url <- "https://bioconductor.org/config.yaml"

  builtin_map <- list(
    "2.1"  = package_version("1.6"),
    "2.2"  = package_version("1.7"),
    "2.3"  = package_version("1.8"),
    "2.4"  = package_version("1.9"),
    "2.5"  = package_version("2.0"),
    "2.6"  = package_version("2.1"),
    "2.7"  = package_version("2.2"),
    "2.8"  = package_version("2.3"),
    "2.9"  = package_version("2.4"),
    "2.10" = package_version("2.5"),
    "2.11" = package_version("2.6"),
    "2.12" = package_version("2.7"),
    "2.13" = package_version("2.8"),
    "2.14" = package_version("2.9"),
    "2.15" = package_version("2.11"),
    "3.0"  = package_version("2.13"),
    "3.1"  = package_version("3.0"),
    "3.2"  = package_version("3.2"),
    "3.3"  = package_version("3.4"),
    "3.4"  = package_version("3.6"),
    "3.5"  = package_version("3.8")
  )

  # -------------------------------------------------------------------
  # Cache

  devel_version <- NULL
  release_version <- NULL
  version_map <- NULL
  yaml_config <- NULL

  # -------------------------------------------------------------------
  # API

  get_yaml_config <- function(forget = FALSE) {

    # Get the contents of https://bioconductor.org/config.yaml, as
    # a character vector, line-wise.
    #
    # If https:// does not work, we re-try with http:// just in case
    # the R build does not have https support.
    #
    # @param forget Whether to update the cached YAML.
    # @return Character vector, one entry for each line in the YAML.

    if (forget || is.null(yaml_config)) {
      new <- tryCatch(readLines(config_url), error = function(x) x)
      if (inherits(new, "error")) {
        http_url <- sub("^https", "http", config_url)
        new <- tryCatch(readLines(http_url), error = function(x) x)
      }
      if (inherits(new, "error")) stop(new)
      yaml_config <<- new
    }

    yaml_config
  }

  set_yaml_config <- function(text) {

    # Set the cached YAML config to the supplied text.
    # This is useful if the file is obtained externally, and you want to
    # use its contents with this API.
    #
    # @param text Contents of the YAML file. Either as a long character
    #   scalar, or a vector with an entry for each line.

    if (length(text) == 1) text <- strsplit(text, "\n", fixed = TRUE)[[1]]
    yaml_config <<- text
  }

  get_release_version <- function(forget = FALSE) {

    # Get the version number of the current Bioconductor release
    #
    # It parses the YAML file at https://bioconductor.org/config.yaml.
    #
    # @param forget Whether to update the cached version and the YAML.
    # @return A [package_version] object, the current Bioconductor
    #   release version.

    if (forget || is.null(release_version)) {
      yaml <- get_yaml_config(forget)
      pattern <- "^release_version: \"(.*)\""
      release_version <<- package_version(
        sub(pattern, "\\1", grep(pattern, yaml, value=TRUE))
      )
    }
    release_version
  }

  get_devel_version <- function(forget = FALSE) {

    # Get the version number of the current Bioconductor development
    # version
    #
    # It parses the YAML file at https://bioconductor.org/config.yaml.
    #
    # @param forget Whether to update the caches version and the YAML.
    # @return A [package_version] object, the current Bioconductor
    #   development version.

    if (forget || is.null(devel_version)) {
      yaml <- get_yaml_config(forget)
      pattern <- "^devel_version: \"(.*)\""
      devel_version <<- package_version(
        sub(pattern, "\\1", grep(pattern, yaml, value=TRUE))
      )
    }
    devel_version
  }

  get_version_map <- function(forget = FALSE) {

    # Create a Bioconductor version -> R version map
    #
    # @param forget Whether to update the cahced YAML.
    # @return A data frame with columns:
    #  * `bioc_version`: Bioconductor version, [package_version] objects.
    #  * `r_version`: Corresponding R version, [package_version] objects.
    #  * `bioc_status`: Current Bioconductor version status, factor, levels:
    #     - `out-of-date`: out of date, old version,
    #     - `release`: current Bioconductor release version,
    #     - `devel`: current Bioconductor devel version.
    #     - `future`: current Bioconductor devel version, in the future
    #       it will map to this R version.

    if (forget || is.null(version_map)) {
      txt <- get_yaml_config(forget)
      grps <- grep("^[^[:blank:]]", txt)
      start <- match(grep("r_ver_for_bioc_ver", txt), grps)
      map <- txt[seq(grps[start] + 1, grps[start + 1] - 1)]
      map <- trimws(gsub("\"", "", sub(" #.*", "", map)))
      pattern <- "(.*): (.*)"
      bioc <- package_version(sub(pattern, "\\1", map))
      r <- package_version(sub(pattern, "\\2", map))
      status <- rep("out-of-date", length(bioc))
      release <- get_release_version()
      devel <- get_devel_version()
      status[bioc == release] <- "release"
      status[bioc == devel] <- "devel"

      # append final version for 'devel' R
      bioc <- c(
        bioc, max(bioc)
        # package_version(paste(unlist(max(bioc)) + 0:1, collapse = "."))
      )
      r <- c(r, package_version(paste(unlist(max(r)) + 0:1, collapse = ".")))
      status <- c(status, "future")

      version_map <<- rbind(
        .VERSION_MAP_SENTINEL,
        data.frame(
          bioc_version = bioc, r_version = r,
          bioc_status = factor(
            status,
            levels = c("out-of-date", "release", "devel", "future")
          )
        )
      )
    }
    version_map
  }

  get_matching_bioc_version <- function(r_version = getRversion(),
                                        forget = FALSE) {

    # Get the matching Bioconductor version for an R version
    #
    # This function tries to avoid making HTTP requests, so it only uses
    # the https://bioconductor.org/config.yaml config file if necessary.
    #
    # @param r_version R version to map, string or [package_version]
    #  object.
    # @param forget Whether to update the cached YAML config. (This is only
    #  used if the function needs the YAML config.)
    # @return Bioconductor version number, [package_version] object.

    minor <- get_minor_r_version(r_version)
    if (minor %in% names(builtin_map)) return(builtin_map[[minor]])

    # If it is not in the map, and we are at the R minor version that
    # comes right after the last one in the map, then we just guess
    # that BioC haven't released before its usual release month.
    # We only do this if the YAML is not available, because if we already
    # have the YAML, then we can do the proper mapping.

    if (minor == "3.6" &&
        Sys.time() < "2019-10-01" &&
        is.null(yaml_config)) return(package_version("3.9"))

    # If we are not in the builtin map, then we need to look this up in
    # YAML data.

    map <- get_version_map(forget = forget)
    mine <- match(package_version(minor), map$r_version)
    if (!is.na(mine)) return(map$bioc_version[mine])

    # If it is not even in the YAML, then it must be some very old
    # or very new version. If old, we fail. If new, we assume bioc-devel.
    if (package_version(minor) < "2.1") {
      stop("R version too old, cannot run Bioconductor")
    }

    get_devel_version()
  }

  get_bioc_version <- function(r_version = getRversion(),
                               forget = FALSE) {

    # Get the Bioconductor version for an R version
    #
    # This function is similar to [get_matching_bioc_version()],
    # but it also observes the `R_BIOC_VERSION` environment variable,
    # which can be set to request a specific Bioconductor version.
    # It this is not set, then it calls [get_matching_bioc_version()].
    #
    # We suggest the you use this function to look up the Bioconductor
    # version that corresponds to the calling R version.
    #
    # @param r_version R version to map, a string or a [package_version]
    #  object.
    # @param forget Whether to update the YAML config file cache.
    #  Passed to [get_matchinf_bioc_version()].
    # @return Bioconductor version number, [package_version] object.

    if (nzchar(v <- Sys.getenv("R_BIOC_VERSION", ""))) {
      return(package_version(v))
    }
    get_matching_bioc_version(r_version, forget = forget)
  }

  get_repos <- function(bioc_version = "auto", forget = FALSE) {

    # Get the URLs of the Bioconductor package repositories for the
    # specified Bioconductor version
    #
    # @param bioc_version The Bioconductor version to use. If it is the
    #   scalar string `"auto"`, then [get_bioc_version()] is used to
    #   look up the desired Bioconductor version.
    # @param forget Whether to update the YAML config cache.
    # @return named character vector of the URLs. Names are
    #   `BioCsoft`, `BioCann`, `BioCexp`, `Biocworkflows` (for newer
    #   Bioconductor versions) and `BioCextra` (for older Bioconductor
    #   versions).

    if (identical(bioc_version, "auto")) {
      bioc_version <- get_bioc_version(getRversion(), forget)
    } else {
      bioc_version <- package_version(bioc_version)
    }
    mirror <- Sys.getenv("R_BIOC_MIRROR", "https://bioconductor.org")
    mirror <- getOption("BioC_mirror", mirror)
    repos <- c(
      BioCsoft      = "{mirror}/packages/{bv}/bioc",
      BioCann       = "{mirror}/packages/{bv}/data/annotation",
      BioCexp       = "{mirror}/packages/{bv}/data/experiment",
      BioCworkflows =
        if (bioc_version >= "3.7") "{mirror}/packages/{bv}/workflows",
      BioCextra     =
        if (bioc_version <= "3.5") "{mirror}/packages/{bv}/extra"
    )

    ## It seems that if a repo is not available yet for bioc-devel,
    ## they redirect to the bioc-release version, so we do not need to
    ## parse devel_repos from the config.yaml file

    sub("{mirror}", mirror, fixed = TRUE,
        sub("{bv}", bioc_version, repos, fixed = TRUE))
  }

  # -------------------------------------------------------------------
  # Internals

  .VERSION_SENTINEL <- local({
    version <- package_version(list())
    class(version) <- c("unknown_version", class(version))
    version
  })

  .VERSION_MAP_SENTINEL <- data.frame(
    bioc_version = .VERSION_SENTINEL,
    r_version = .VERSION_SENTINEL,
    bioc_status = factor(
      factor(),
      levels = c("out-of-date", "release", "devel", "future")
    )
  )

  get_minor_r_version <- function (x) {
    x <- package_version(x)
    vapply(unclass(x), function(x) paste(x[1:2], collapse = "."),
           character(1))
  }

  # -------------------------------------------------------------------

  structure(
    list(
      .internal = environment(),
      get_yaml_config = get_yaml_config,
      set_yaml_config = set_yaml_config,
      get_release_version = get_release_version,
      get_devel_version = get_devel_version,
      get_version_map = get_version_map,
      get_matching_bioc_version = get_matching_bioc_version,
      get_bioc_version = get_bioc_version,
      get_repos = get_repos
    ),
    class = c("standalone_bioc", "standalone"))
})
