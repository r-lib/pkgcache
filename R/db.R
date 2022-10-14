
db_cols <- read.table(
  header = TRUE,
  stringsAsFactors = FALSE,
  textConnection(
    "column                type
     package               TEXT
     version               TEXT
     depends               TEXT
     imports               TEXT
     suggests              TEXT
     linkingto             TEXT
     enhances              TEXT
     license               TEXT
     license_restricts_use BOOLEAN
     license_is_foss       BOOLEAN
     os_type               TEXT
     priority              TEXT
     repodir               TEXT
     rversion              TEXT
     platform              TEXT
     needscompilation      BOOLEAN
     ref                   TEXT
     type                  TEXT
     direct                BOOLEAN
     status                TEXT
     target                TEXT
     mirror                TEXT
     sources               TEXT
     filesize              INTEGER
     sha256                TEXT
     sysreqs               TEXT
     built                 TEXT
     published             TEXT
     md5sum                TEXT
     path                  TEXT
"))

db_create <- function(path) {
  path <- enc2utf8(path)
  q <- paste0(
    "CREATE TABLE package (",
    paste(db_cols$column, db_cols$type, collapse = ", "),
    ");"
  )

  .Call(c_sql3_create_db, path, q)
}

db_add_packages <- function(path, packages_path, meta_path, mirror,
                            repodir, platform, type) {
  path <- enc2utf8(path)
  mirror <- enc2utf8(mirror)

  packages <- parse_packages(packages_path)
  names(packages) <- tolower(names(packages))
  keep <- intersect(db_cols$column, names(packages))
  packages2 <- packages[, keep]

  packages2$repodir <- rep(repodir, nrow(packages2))
  packages2$platform <- rep(platform, nrow(packages2))
  packages2$ref <- packages2$package
  packages2$direct <- rep(FALSE, nrow(packages2))
  packages2$status <- rep("OK", nrow(packages2))
  packages2$mirror <- rep(mirror, nrow(packages2))
  packages2$type <- rep(type, nrow(packages2))
  packages2$target <- packages_make_target(
    platform, repodir, packages2$package, packages2$version,
    packages2[["file"]], packages2[["path"]]
  )

  meta <- read_metadata_file(meta_path)
  if (!is.null(meta)) {

  }

  # TODO: platform
  # TODO: rversion

  query <- paste0(
    "INSERT INTO package (",
    paste(names(packages2), collapse = ", "),
    ") VALUES (",
    paste(rep("?", ncol(packages2)), collapse = ", "),
    ")"
  )

  print(setdiff(db_cols$column, names(packages2)))
  .Call(c_sql3_add_packages, path, packages2, query)
}
