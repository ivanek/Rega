## Generate the object with EGA enums for offline use. Requires EGA credentials.
## Author: Igor Cervenka
## Licence: Artistic-2.0
tryCatch(
  {
    ega <- Rega::create_client(Rega::extract_api(), verbosity = 0)
    enums <- lapply(ega$get__enums()$enums, function(x) {
      apply(
        Rega::get_enum(ega, x),
        1,
        \(row) paste0(trimws(row), collapse = "--")
      )
    }) |>
      setNames(ega$get__enums()$enums)

    saveRDS(
      enums,
      file.path("inst", "extdata", "ega_enums.rds"),
      overwrite = TRUE
    )
  }

)
