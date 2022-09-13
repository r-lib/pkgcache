
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
  file.copy(out, path)
  out
}

cran_app <- function(packages = NULL,
                     log = interactive(),
                     options = list()) {

  packages <- packages %||% data.frame(
    stringsAsFactors = FALSE,
    Package = character()
  )

  if (!"Package" %in% names(packages)) {
    packages$Package <- paste0("pkg", seq_len(nrow(packages)))
  }

  app <- webfakes::new_app()

  # -----------------------------------------------------------------------

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

  # -----------------------------------------------------------------------

  app$locals$packages <- packages
  dir.create(app$locals$repo <- repo <- tempfile())
  reg.finalizer(
    app,
    function(obj) unlink(obj$locals$repo, recursive = TRUE),
    TRUE
  )

  app$use("repo" = webfakes::mw_static(repo))

  # -----------------------------------------------------------------------

  dir_source <- utils::contrib.url("", "source")
  mkdirp(repo_source <- file.path(repo, dir_source))

  packages$file <- character(nrow(packages))
  for (i in seq_len(nrow(packages))) {
    fn <- make_dummy_package(packages[i, , drop = FALSE], repo_source)
    packages$file[i] <- fn
  }

  # -----------------------------------------------------------------------

  if (!isTRUE(options$no_packages)) {
    file.create(file.path(repo_source, "PACKAGES"))
    tools::write_PACKAGES(repo_source)
  }

  if (isTRUE(options$no_packages_gz)) {
    file.remove(file.path(repo_source, "PACKAGES.gz"))
  }

  if (isTRUE(options$no_packages_rds)) {
    file.remove(file.path(repo_source, "PACKAGES.rds"))
  }

  # -----------------------------------------------------------------------

  if (!isTRUE(options$no_metadata)) {
    meta <- data.frame(
      stringsAsFactors = FALSE,
      file = packages$file,
      size = file.size(file.path(repo_source, packages$file)),
      sha = unname(tools::md5sum(file.path(repo_source, packages$file))),
      sysreqs = packages$SystemRequirements %||% rep(NA_character_, nrow(packages)),
      built = if (nrow(packages)) NA_character_ else character(),
      published = if (nrow(packages)) format(Sys.time()) else character()
    )
    outcon <- gzcon(file(file.path(repo_source, "METADATA2.gz"), "wb"))
    utils::write.csv(meta, outcon, row.names = FALSE)
    close(outcon)
  }

  # -----------------------------------------------------------------------

  app
}

dcf <- function(txt) {
  txt <- gsub("\n[ ]+", "\n", txt)
  as.data.frame(read.dcf(textConnection(txt)))
}
