# parse_description

    Code
      parse_description(tempfile())
    Condition
      Error in `parse_description()`:
      ! Cannot open file `<tempdir>/<tempfile>` (system error 2, The system cannot find the file specified.
      ) @lib.c:<linum> (pkgcache__read_file_raw) @lib.c:<linum> (pkgcache_parse_description)

# parse_packages, errors

    Code
      parse_packages(tempfile(), type = "uncompressed")
    Condition
      Error in `parse_packages()`:
      ! Cannot open file `<tempdir>/<tempfile>` (system error 2, The system cannot find the file specified.
      ) @lib.c:111 (pkgcache__read_file_raw)

