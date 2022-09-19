
# nocov start

make_dummy_package <- function(data, path) {
  package <- data$Package
  data$Version <- data$Version %||% "1.0.0"
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_dir(tmp)
  mkdirp(package)
  file.create(file.path(package, "NAMESPACE"))
  write.dcf(data, file.path(package, "DESCRIPTION"))
  suppressMessages(utils::capture.output(
    out <- asNamespace("tools")$.build_packages(args = package)
  ))
  unlink(package, recursive = TRUE)
  out <- dir()
  if (length(out) != 1) stop("Failed to build package ", package, " :(")
  mkdirp(path)
  file.copy(out, path)
  out
}

standardize_dummy_packages <- function(packages) {
  packages <- packages %||% data.frame(
    stringsAsFactors = FALSE,
    Package = character()
  )

  if (!"Package" %in% names(packages)) {
    packages$Package <- paste0("pkg", seq_len(nrow(packages)))
  }

  if (!"Version" %in% names(packages)) {
    packages$Version <- if (nrow(packages)) "1.0.0" else character()
  } else {
    packages$Version[is.na(packages$Version)] <- "1.0.0"
  }

  packages
}

make_dummy_repo <- function(repo, packages = NULL, options = list()) {

  mkdirp(repo)

  packages <- standardize_dummy_packages(packages)

  if (!is.null(options$repo_prefix)) {
    repo <- file.path(repo, options$repo_prefix)
  }
  mkdirp(repo_source <- file.path(repo, utils::contrib.url("", "source")))

  extra <- packages
  extra$file <- character(nrow(extra))

  latest <- tapply(
    extra$Version,
    extra$Package,
    function(x) as.character(max(package_version(x))),
    simplify = TRUE
  )
  extra$archive <- latest[extra$Package] != extra$Version

  for (i in seq_len(nrow(packages))) {
    pkg_dir <- if (extra$archive[i]) {
      file.path(repo_source, "Archive", packages$Package[i])
    } else {
      repo_source
    }

    fn <- make_dummy_package(packages[i, , drop = FALSE], pkg_dir)
    extra$file[i] <- fn
  }

  if (!isTRUE(options$no_packages)) {
    file.create(file.path(repo_source, "PACKAGES"))
    tools::write_PACKAGES(repo_source)
  }

  if (isTRUE(options$no_packages_gz)) {
    file.remove(file.path(repo_source, "PACKAGES.gz"))
  } else if (file.exists(file.path(repo_source, "PACKAGES.gz"))) {
    # if empty

  }

  if (isTRUE(options$no_packages_rds)) {
    file.remove(file.path(repo_source, "PACKAGES.rds"))
  }

  if (!isTRUE(options$no_metadata)) {
    current <- extra[!extra$archive,, drop = FALSE]
    meta <- data.frame(
      stringsAsFactors = FALSE,
      file = current$file,
      size = file.size(file.path(repo_source, current$file)),
      sha = cli::hash_file_sha256(file.path(repo_source, current$file)),
      sysreqs = current$SystemRequirements %||% rep("NA", nrow(current)),
      built = if (nrow(current)) "NA" else character(),
      published = if (nrow(current)) format(Sys.time()) else character()
    )
    outcon <- gzcon(file(file.path(repo_source, "METADATA2.gz"), "wb"))
    utils::write.csv(meta, outcon, row.names = FALSE)
    close(outcon)
  }

  if (!isTRUE(options$no_archive)) {
    archive <- extra[extra$archive,, drop = FALSE]
    adf <- list()
    adir <- file.path(repo_source, "Archive")
    if (file.exists(adir)) {
      adirs <- dir(adir)
      adf <- lapply(adirs, function(d) {
        pkgs <- dir(file.path(adir, d), full.names = TRUE)
        fi <- file.info(pkgs)
        rownames(fi) <- basename(rownames(fi))
        fi
      })
      names(adf) <- adirs
    }

    mkdirp(file.path(repo_source, "Meta"))
    saveRDS(adf, file.path(repo_source, "Meta", "archive.rds"))
  }

  invisible()
}

cran_app <- function(packages = NULL,
                     log = interactive(),
                     options = list()) {

  app <- webfakes::new_app()

  # Log requests by default
  if (log) app$use("logger" = webfakes::mw_log())

  # Parse all kinds of bodies
  app$use("json body parser" = webfakes::mw_json())
  app$use("text body parser" = webfakes::mw_text(type = c("text/plain", "application/json")))
  app$use("multipart body parser" = webfakes::mw_multipart())
  app$use("URL encoded body parser" = webfakes::mw_urlencoded())

  # Add etags by default
  app$use("add etag" = webfakes::mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  app$locals$repo <- repo <- tempfile()
  reg.finalizer(
    app,
    function(obj) unlink(obj$locals$repo, recursive = TRUE),
    TRUE
  )

  app$use("repo" = webfakes::mw_static(repo))
  make_dummy_repo(repo, packages, options)

  app
}

dcf <- function(txt) {
  txt <- gsub("\n[ ]+", "\n", txt)
  as.data.frame(read.dcf(textConnection(txt)), stringsAsFactors = FALSE)
}

fix_port <- function(x) {
  gsub("http://127[.]0[.]0[.]1:[0-9]+", "http://127.0.0.1:<port>", x)
}

bioc_app <- function(packages = NULL,
                     log = interactive(),
                     options = list()) {

  packages <- standardize_dummy_packages(packages)

  bioc_version <- options$bioc_version %||% bioconductor$get_bioc_version()
  options$no_metadata <- options$no_metadata %||% TRUE
  options$no_archive <- options$no_archive %||% TRUE

  packages$bioc_repo <- packages$bioc_repo %||%
    if (nrow(packages)) "soft" else character()
  packages$bioc_repo[is.na(packages$bioc_repo)] <- "soft"
  bioc_repo <- packages$bioc_repo
  packages <- packages[, setdiff(names(packages), "bioc_repo"), drop = FALSE]

  # BioCsoft
  options$repo_prefix <- sprintf("packages/%s/bioc", bioc_version)
  pkg_soft <- packages[bioc_repo == "soft",, drop = FALSE]
  app <- cran_app(pkg_soft, log, options)

  # BioCann
  options$repo_prefix <- sprintf("packages/%s/data/annotation", bioc_version)
  pkg_ann <- packages[bioc_repo == "ann",, drop = FALSE]
  make_dummy_repo(app$locals$repo, pkg_ann, options)

  # BioCexp
  options$repo_prefix <- sprintf("packages/%s/data/experiment", bioc_version)
  pkg_exp <- packages[bioc_repo == "exp",, drop = FALSE]
  make_dummy_repo(app$locals$repo, pkg_ann, options)

  # BioCworkflows
  options$repo_prefix <- sprintf("packages/%s/workflows", bioc_version)
  pkg_workflows <- packages[bioc_repo == "workflows",, drop = FALSE]
  make_dummy_repo(app$locals$repo, pkg_workflows, options)

  config <- system.file("fixtures", "bioc-config.yaml", package = "pkgcache")
  if (config == "") {
    warning("Cannot find 'bioc-config.yaml' in pkgcache")
  } else {
    file.copy(config, file.path(app$locals$repo, "config.yaml"))
  }

  app
}

# nocov end
