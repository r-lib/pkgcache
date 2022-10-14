
packages_gz_cols <- function()  {
  list(
    pkgs = c("ref", "type", "direct", "status", "package", "version",
             "platform", "rversion", "repodir", "sources", "target",
             "needscompilation", "priority", "filesize", "sha256",
             "sysreqs", "built", "published"),
    deps = c("upstream", "idx", "ref", "type", "package", "op", "version")

  )
}

#' @importFrom tools file_ext

read_packages_file <- function(path, mirror, repodir, platform,
                               type = "standard", meta_path = NA_character_,
                               ..., .list = list()) {

  # We might have empty PACKAGES.gz files, we treat them as empty here
  if (file.exists(path) && file.size(path) == 0) {
    pkgs <- data_frame()
  } else {
    pkgs <- parse_packages(path)
  }
  meta <- read_metadata_file(meta_path)
  extra <- c(
    list(repodir = repodir),
    list(...), .list)
  assert_that(all_named(extra))
  pkgs[names(extra)] <-
    if (nrow(pkgs)) extra else replicate(length(extra), character())
  names(pkgs) <- tolower(names(pkgs))

  ## If Windows, then we need to check which binary has i386 support
  if (nrow(pkgs)) {
    if (platform %in% c("i386+x86_64-w64-mingw32",
                        "i386-w64-mingw32", "x86_64-w64-mingw32")) {
      pkgs$platform <- "i386+x86_64-w64-mingw32"
      if ("archs" %in% colnames(pkgs)) {
        archs <- gsub(" ", "", fixed = TRUE, pkgs$archs)
        p32 <- !is.na(archs) & archs == "i386"
        pkgs$platform[p32] <- "i386-w64-mingw32"
        p64 <- !is.na(archs) & archs == "x64"
        pkgs$platform[p64] <- "x86_64-w64-mingw32"
      }
    } else {
      pkgs$platform <- if (nrow(pkgs)) platform
    }
  } else {
    pkgs$platform <- character()
  }

  if (! "needscompilation" %in% names(pkgs)) {
    pkgs$needscompilation <- if (! "built" %in% names(pkgs)) {
      if (nrow(pkgs)) NA_character_ else character()
    } else {
      ifelse(is.na(pkgs$built), NA_character_, "no")
    }
  }

  if (! "priority" %in% names(pkgs)) {
    pkgs$priority <- rep(NA_character_, nrow(pkgs))
  }

  if (!nrow(pkgs)) {
    pkgs$package <- character()
    pkgs$version <- character()
  }
  pkgs$ref <- pkgs$package
  pkgs$type <- if (nrow(pkgs)) type else character()
  pkgs$direct <- if (nrow(pkgs)) FALSE else logical()
  pkgs$status <- if (nrow(pkgs)) "OK" else character()
  pkgs$target <- packages_make_target(
    platform, repodir, pkgs$package, pkgs$version, pkgs[["file"]], pkgs[["path"]])
  pkgs$mirror <- if (nrow(pkgs)) mirror else character()
  pkgs$sources <- packages_make_sources(
    mirror, platform, pkgs$target, repodir, pkgs$package, pkgs$version,
    type, pkgs$downloadurl)

  if (!is.null(meta)) {
    metatarget <- paste0(repodir, "/", meta$file)
    map <- match(pkgs$target, metatarget)
    pkgs$filesize  <- meta$size[map]
    pkgs$sha256    <- meta$sha[map]
    pkgs$sysreqs   <- as.character(meta$sysreqs[map])
    pkgs$built     <- as.character(meta$built[map])
    pkgs$published <- meta$published[map]
    pkgs$published[pkgs$published == ""] <- NA_character_
    pkgs$published <- as.POSIXct(pkgs$published, tz = "GMT")

    # fall back to previous version for filesize and sysreqs
    nameonly <- function(x) sub("_[^_]*$", "", x)
    map2 <- match(nameonly(pkgs$target), nameonly(metatarget))
    filesize2 <- meta$size[map2]
    sysreqs2 <- as.character(meta$sysreqs[map2])
    pkgs$filesize <- ifelse(is.na(pkgs$filesize), filesize2, pkgs$filesize)
    pkgs$sysreqs <- ifelse(is.na(pkgs$sysreqs), sysreqs2, pkgs$sysreqs)

  } else {
    pkgs$filesize <- rep(NA_integer_, nrow(pkgs))
    pkgs$sha256 <- pkgs$sysreqs <- pkgs$built <- pkgs$published <-
        rep(NA_character_, nrow(pkgs))
  }

  # If we only want one Windows platform, then filter here
  if (platform %in% c("i386-w64-mingw32", "x86_64-w64-mingw32")) {
    drop <- pkgs$platform != platform &
      pkgs$platform != "i386+x86_64-w64-mingw32"
    if (any(drop)) {
      pkgs <- pkgs[!drop, ]
    }
  }

  deps <- packages_parse_deps(pkgs)
  pkgs_deps <- split(
    deps[,-(1:2)], factor(deps$idx, levels = seq_len(nrow(pkgs))))
  pkgs$deps <- unname(pkgs_deps)
  list(pkgs = pkgs, deps = deps)
}

#' @importFrom utils read.csv

read_metadata_file <- function(path) {
  if (is.na(path)) return(NULL)
  on.exit(tryCatch(close(con), error = function(x) NULL), add = TRUE)
  tryCatch(suppressWarnings({
    md <- read.csv(
      con <- gzfile(path, open = "r"),
      stringsAsFactors = FALSE
    )
    if ("filesize" %in% names(md)) {
      md$filesize <- as.integer(md$filesize)
    }
    md
  }), error = function(e) NULL)
}

packages_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), tolower(dep_types()))
  ## as.character is for empty data frame, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- re_match(sp,
      paste0("^\\s*(?<package>[^(\\s]+)\\s*",
             "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"))
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$package[parsed$idx]
    parsed <- parsed[, c("upstream", "idx", "ref", "type", "package",
                         "op", "version")]
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- data_frame(
      upstream = character(),
      idx = integer(),
      ref = character(),
      type = character(),
      package = character(),
      version = character(),
      op = character()
    )
  }

  parsed
}

packages_make_target <- function(platform, repodir, package, version,
                                 file, path) {

  assert_that(
    is_string(platform),
    is_string(repodir),
    is_character(package),
    is_character(version), length(version) == length(package),
    is.null(file) || (is.character(file) && length(file) == length(package)),
    is.null(path) || (is.character(path) && length(path) == length(package))
  )

  res <- rep(NA_character_, length(package))
  ext <- get_cran_extension(platform)

  ## 'File' field, if present
  if (!is.null(file)) {
    wh <- !is.na(file)
    res[wh] <- paste0(repodir, "/", file[wh])
  }

  ## 'Path' field, if present
  if (!is.null(path)) {
    wh <- is.na(res) & !is.na(path)
    res[wh] <- paste0(repodir, "/", path[wh], "/", package[wh], "_",
                      version[wh], ext)
  }

  ## Otherwise default
  wh <- is.na(res)
  res[wh] <- paste0(repodir, "/", package[wh], "_", version[wh], ext)

  res
}

packages_make_sources <- function(mirror, platform, target, repodir,
                                  package, version, type, downloadurl) {

  assert_that(
    is_string(mirror),
    is_string(platform),
    is_character(target),
    is_string(repodir),
    is_character(package),
    is_character(version), length(version) == length(package),
    is.null(downloadurl) || is.character(downloadurl)
  )

  if (!length(package)) return(list())

  url <- paste0(mirror, "/", target)

  os <- parse_platform(platform)$os
  srcs <- if (type == "cran" && !is.na(os) && grepl("^darwin", os)) {
    macurl <- paste0("https://mac.r-project.org/", target)
    zip_vecs(url, macurl)

  } else if (type == "cran" && platform == "source") {
    url2 <- paste0(mirror, "/", repodir, "/Archive/", package, "/", package, "_",
                   version, ".tar.gz")
    zip_vecs(url, url2)

  } else {
    as.list(url)
  }

  if (!is.null(downloadurl)) {
    srcs[!is.na(downloadurl)] <- as.list(na_omit(downloadurl))
  }

  srcs
}

merge_packages_data <- function(..., .list = list()) {
  pkgslist <- c(list(...), .list)

  pkgs <- rbind_expand(.list = lapply(pkgslist, "[[", "pkgs"))

  ## Need to shift deps indices first to merge deps
  num_pkgs <- viapply(pkgslist, function(x) nrow(x$pkgs), USE.NAMES = FALSE)
  shifts <- c(0L, cumsum(num_pkgs))
  for (i in seq_along(pkgslist)) {
    pkgslist[[i]]$deps$idx <- pkgslist[[i]]$deps$idx + shifts[i]
  }
  deps <- rbind_expand(.list = lapply(pkgslist, "[[", "deps"))

  list(pkgs = pkgs, deps = deps)
}

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as_data_frame(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE),
        names = miss_cols))
      data[[i]] <- as_data_frame(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}
