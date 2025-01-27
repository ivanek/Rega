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
#' @examples
#' # Template file is empty, trying to parse it as is will fail
#' try(
#'   default_parser(
#'     system.file("extdata/ega_full_template_v2.xlsx", package = "Rega")
#'   )
#' )
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
    type = "aliases_formatter", params = list(na_omit = TRUE)
  )

  # Read the xlsx file
  sm <- setNames(lapply(
    sheets,
    \(x) read_xlsx(metadata_file, sheet = x, col_names = FALSE)
  ), label_to_api_name(sheets))

  # Parse the xlsx sheets
  pm <- setNames(lapply(names(sm), function(x) {
    fp <- get_formatter_params(x, params)
    get_formatter(x, params)(sm[[x]], fp)
  }), names(sm))

  # Cleanup parsed metadata ----------------------------------------------------
  pm <- lapply(pm, function(x) {
    if (is.data.frame(x)) {
      x <- x[, colSums(!is.na(x)) > 0] # Remove columns that are all NA
    }
    x
  })

  # Checks if the Aliases have an analysis entry or if there are Analysis Files
  # specified, if not, deletes both Analyses and Analyses Files sheet
  if (is.null(pm$aliases$analyses) ||
    length(pm$aliases$analyses) == 0 ||
    dim(pm$analysis_files)[1] == 0) {
    pm$analysis_files <- NULL
    pm$analyses <- NULL
  }

  # Merge runs with file sheet, replace file with ega_file
  files_lut <- setNames(pm$files$ega_file, pm$files$file)
  pm$runs <- lut_add(pm$runs, "files", "files", files_lut)

  # Merge analysis with file sheet, files are in nested column
  # If analyses sheet is present
  if ("analyses" %in% names(pm)) {
    af_lut <- setNames(pm$analysis_files$ega_file, pm$analysis_files$file)
    pm$analyses <- lut_add(pm$analyses, "files", "files", af_lut)
  }

  # Link the sheets with extra nested info
  if (length(params$linked_sheets) > 0) {
    pm <- Reduce(
      function(m, sheet_name) link_sheet(m, sheet_name),
      label_to_api_name(params$linked_sheets),
      init = pm
    )
  }

  # Separate the delimited columns from metadata into lists for JSON conversion
  if (length(params$delimited_columns$names) > 0) {
    pm <- Reduce(
      function(m, column_name) {
        process_delimited_column(
          m,
          column_name,
          params$delimited_columns$separator
        )
      },
      label_to_api_name(params$delimited_columns$names),
      init = pm
    )
  }

  # Get only instrument ids for experiments (API: mandatory)
  pm$experiments$instrument_model_id <-
    as.integer(str_split_i(pm$experiments$instrument_model_id, "--", 1))

  # Get only file type ids for runs (API: mandatory)
  pm$runs$run_file_type <- str_split_i(pm$runs$run_file_type, "--", 1)

  # If analyses sheet is present
  if ("analyses" %in% names(pm)) {
    # Get only genome IDs (API: mandatory)
    pm$analyses$genome_id <-
      as.integer(str_split_i(pm$analyses$genome_id, "--", 2))

    # Get only chromosome ids for analysis (API: optional)
    chr_list <- format_chromosomes(pm)
    pm$analyses$chromosomes <- chr_list

    # Replace NAs in experiment_types with empty lists
    # To not mess with the link_sheet function, code for this specific case
    # is not included in fold_columns
    pm$analyses$experiment_types <-
      lapply(pm$analyses$experiment_types, function(x) {
        if (is.na(x)) {
          return(list())
        } else {
          return(x)
        }
      })
  }

  return(pm)
}
