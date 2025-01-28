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
#'     default_parser(
#'         system.file("extdata/ega_full_template_v2.xlsx", package = "Rega")
#'     )
#' )
#'
#' @export
default_parser <- function(metadata_file, param_file = NULL) {
    # Run metadata parsing -----------------------------------------------------
    # If not specified, load the default parameter yaml file
    if (is.null(param_file)) {
        param_file <- system.file(
            "extdata/default_parser_params.yaml",
            package = "Rega"
        )
    }
    p <- read_yaml(param_file)
    sheets <- c(p$submission_sheets, p$linked_sheets, "Select Input Data")

    # Read the xlsx file
    sm <- setNames(lapply(
        sheets,
        \(x) read_xlsx(metadata_file, sheet = x, col_names = FALSE)
    ), label_to_api_name(sheets))

    # Parse the xlsx sheets
    pm <- setNames(lapply(names(sm), function(x) {
        fp <- get_formatter_params(x, p)
        get_formatter(x, p)(sm[[x]], fp)
    }), names(sm))

    # Cleanup parsed metadata --------------------------------------------------
    pm <- lapply(pm, function(x) {
        if (is.data.frame(x)) {
            x <- x[, colSums(!is.na(x)) > 0] # Remove columns that are all NA
        }
        x
    })

    if (!.has_analyses(pm)) pm$analysis_files <- pm$analyses <- NULL

    # Merge runs with file sheet, replace file with ega_file
    files_lut <- setNames(pm$files$ega_file, pm$files$file)
    pm$runs <- lut_add(pm$runs, "files", "files", files_lut)

    # Merge analysis with file sheet, files are in nested column
    # If analyses sheet is present
    # if ("analyses" %in% names(pm)) {
    #   af_lut <- setNames(pm$analysis_files$ega_file, pm$analysis_files$file)
    #   pm$analyses <- lut_add(pm$analyses, "files", "files", af_lut)
    # }

    # Link the sheets with extra nested info
    if (length(p$linked_sheets) > 0) {
        pm <- Reduce(
            function(m, sheet_name) link_sheet(m, sheet_name),
            label_to_api_name(p$linked_sheets),
            init = pm
        )
    }

    # Separate the delimited columns from metadata into lists for JSON
    # conversion
    if (length(p$delimited_columns$names) > 0) {
        sep <- p$delimited_columns$separator
        pm <- Reduce(
            function(m, column_name) {
                process_delimited_column(m, column_name, sep)
            },
            label_to_api_name(p$delimited_columns$names),
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
        # Merge analysis with file sheet, files are in nested column
        af_lut <- setNames(pm$analysis_files$ega_file, pm$analysis_files$file)
        pm$analyses <- lut_add(pm$analyses, "files", "files", af_lut)
        # Get only genome IDs (API: sometimes mandatory)
        if("genome_id" %in% names(pm$analyses)) {
            pm$analyses$genome_id <-
                as.integer(str_split_i(pm$analyses$genome_id, "--", 2))
        }
        # Get only chromosome ids for analysis (API: optional)
        pm$analyses$chromosomes <- format_chromosomes(pm)
        # Replace NAs in experiment_types with empty lists
        # Not to mess with the link_sheet function, code for this specific case
        # is not included in fold_columns
        if("experiment_types" %in% names(pm$analyses)) {
            pm$analyses$experiment_types <-
                na_to_empty_list(pm$analyses$experiment_types)
        }
    }

    return(pm)
}

#' Check for Presence of Analyses Sheet in Metadata
#'
#' Determines whether analyses or analysis files are specified in the provided
#' metadata.
#'
#' @param meta A list containing metadata with elements \code{analyses} and
#'   \code{analysis_files}.
#'
#' @return A logical value: \code{TRUE} if analyses are absent or empty,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' meta <- list(
#'   aliases = list(analyses = c("A1", "A2")),
#'   analysis_files = data.frame(name = character(0))
#' )
#' Rega:::.has_analyses(meta)
#'
#' @keywords internal
.has_analyses <- function(meta) {
    # Checks if the Aliases have an analysis entry or if there are Analysis
    # Files specified, if not, deletes both Analyses and Analyses Files sheet
    if (is.null(meta$aliases$analyses) ||
        length(meta$aliases$analyses) == 0 ||
        dim(meta$analysis_files)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' Convert NA Values to Empty Lists
#'
#' Replaces \code{NA} values in a list with empty lists, preserving the original
#' structure of the list.
#'
#' @param l A list containing elements that may include \code{NA} values.
#'
#' @return A list where any \code{NA} values have been replaced with empty
#'   lists.
#'
#' @examples
#' input_list <- list(1, NA, "text", NA)
#' na_to_empty_list(input_list)
#'
#' @export
na_to_empty_list <- function(l) {
    lapply(l, function(x) {
        if (is.na(x)) {
            return(list())
        } else {
            return(x)
        }
    })
}
