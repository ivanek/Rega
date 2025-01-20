#' Parser for a Default EGA Excel Template
#'
#' This function parses the `extdata/template.xlsx` using the bundled parser
#' parameter file in `extdata/default_parser_params.yaml` to extract
#' information for EGA submission into format that can be easily passed
#' into EGA API endpoints.
#'
#' @param metadata_file Character. Path to a default template xlsx file
#'   containing the submission metadata information.
#' @param param_file Character. Path to a yaml file with parameters for parser.
#'
#' @return List of data frames or lists. Submission information parsed from the
#' xlsx file.
#'
#' @importFrom yaml read_yaml
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom stringr str_split_i
#'
#' @export
default_parser <- function(metadata_file, param_file = NULL) {
  # Run metadata parsing -------------------------------------------------------
  # If not specified, load the default parameter yaml file
  if (is.null(param_file)) {
    param_file <- system.file(
      "extdata/default_parser_params.yaml",
      package = "Rega"
    )
  }

  params <- read_yaml(param_file)
  sheets <- c(
    params$submission_sheets,
    params$linked_sheets,
    "Select Input Data"
  )
  params$formatter$select_input_data <- list(
    type = "aliases_formatter",
    params = list(
      na_omit = TRUE
    )
  )

  # Read the xlsx file
  submission_meta <- lapply(
    sheets,
    \(x) read_xlsx(metadata_file, sheet = x, col_names = FALSE)
  ) |>
    setNames(label_to_api_name(sheets))

  # Parse the xlsx sheets
  parsed_metadata <- lapply(names(submission_meta), function(x) {
    fp <- get_formatter_params(x, params)
    df <- get_formatter(x, params)(submission_meta[[x]], fp)
    df
  }) |>
    setNames(names(submission_meta))

  # Cleanup parsed metadata ----------------------------------------------------
  parsed_metadata <- lapply(parsed_metadata, function(x) {
    if (is.data.frame(x)) {
      # Replace all empty characters with NA. Can happen e.g. if the drop-down
      # selection menu for Extra Attributes has been replaced by empty excel
      # cell or deleted
      # Remove columns that are all NA
      x <- x[, colSums(!is.na(x)) > 0]
    }
    x
  })

  # Checks if the Aliases have an analysis entry or if there are Analysis Files
  # specified, if not, deletes both Analyses and Analyses Files sheet
  if (is.null(parsed_metadata$aliases$analyses) ||
    length(parsed_metadata$aliases$analyses) == 0 ||
    dim(parsed_metadata$analysis_files)[1] == 0) {
    parsed_metadata$analysis_files <- NULL
    parsed_metadata$analyses <- NULL
  }

  # Merge runs with file sheet, replace file with ega_file
  files_lut <- setNames(
    parsed_metadata$files$ega_file,
    parsed_metadata$files$file
  )

  parsed_metadata$runs <- lut_add(
    parsed_metadata$runs,
    "files",
    "files",
    files_lut
  )

  # Merge analysis with file sheet, files are in nested column
  # If analyses sheet is present
  if ("analyses" %in% names(parsed_metadata)) {
    analysis_files_lut <- setNames(
      parsed_metadata$analysis_files$ega_file,
      parsed_metadata$analysis_files$file
    )

    parsed_metadata$analyses <- lut_add(
      parsed_metadata$analyses,
      "files",
      "files",
      analysis_files_lut
    )
  }

  # Link the sheets with extra nested info
  if (length(params$linked_sheets) > 0) {
    parsed_metadata <- Reduce(
      function(m, sheet_name) link_sheet(m, sheet_name),
      label_to_api_name(params$linked_sheets),
      init = parsed_metadata
    )
  }

  # Separate the delimited columns from metadata into lists for JSON conversion
  if (length(params$delimited_columns$names) > 0) {
    parsed_metadata <- Reduce(
      function(m, column_name) {
        process_delimited_column(
          m,
          column_name,
          params$delimited_columns$separator
        )
      },
      label_to_api_name(params$delimited_columns$names),
      init = parsed_metadata
    )
  }

  # Get only instrument ids for experiments (API: mandatory)
  parsed_metadata$experiments$instrument_model_id <-
    as.integer(
      str_split_i(parsed_metadata$experiments$instrument_model_id, "--", 1)
    )

  # Get only file type ids for runs (API: mandatory)
  parsed_metadata$runs$run_file_type <-
    str_split_i(parsed_metadata$runs$run_file_type, "--", 1)

  # If analyses sheet is present
  if ("analyses" %in% names(parsed_metadata)) {
    # Get only genome IDs (API: mandatory)
    parsed_metadata$analyses$genome_id <-
      as.integer(str_split_i(parsed_metadata$analyses$genome_id, "--", 2))

    # Get only chromosome ids for analysis (API: optional)
    chr_list <- format_chromosomes(parsed_metadata)
    parsed_metadata$analyses$chromosomes <- chr_list

    # Replace NAs in experiment_types with empty lists
    # To not mess with the link_sheet function, code for this specific case
    # is not included in fold_columns
    parsed_metadata$analyses$experiment_types <-
      lapply(parsed_metadata$analyses$experiment_types, function(x) {
        if (is.na(x)) {
          return(list())
        } else {
          return(x)
        }
      })
  }

  return(parsed_metadata)
}
