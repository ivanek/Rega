# Submission metadata validation -----------------------------------------------

#' Validator for Default Parser
#'
#' Used to validate internal consistency of submission metadata parsed using
#' the default parser. Performs several checks on EGA dataset for submission,
#' ensuring that aliases for studies, experiments, samples, runs, analyses and
#' datasets are  are properly linked, as they will be replaced with provisional
#' or accession IDs during submission process. Displays a success message if
#' all validation passed or a summary message if validation failed. In addition
#' it returns a data frame with validation details.
#'
#' @param meta List of data frames. Correspond to tables of EGA submission.
#' @param aliases List of lists. Aliases that should present in the
#' EGA tables.  If `NULL`, the function will attempt to locate it in the
#' `meta` parameter. Defaults to `NULL`
#'
#' @return Data frame. Validator object that includes all performed validations
#' and their statistics (number of passes, fails and NAs, or whether errors or
#' warnings were encountered during validation)
#'
#' @importFrom validate summary
#'
#' @examples
#' minimal_metadata <- list(
#'     aliases = list(
#'         studies = "Study1", experiments = "Experiment1",
#'         datasets = "Dataset1", samples = "Sample1", runs = "Run1",
#'         analyses = "Analysis1"
#'     ),
#'     files = tibble::tibble(
#'         file = "raw.fastq.gz", ega_file = list("raw.fastq.gz.c4gh")
#'     ),
#'     submission = tibble::tibble(title = "Submission"),
#'     studies = tibble::tibble(
#'         study = "Study1", title = "Study Title",
#'         description = "Study Description",
#'         study_type = "Whole Genome Sequencing"
#'     ),
#'     samples = tibble::tibble(
#'         alias = "Sample1", phenotype = "wild-type",
#'         biological_sex = "female", subject_id = "ID1"
#'     ),
#'     experiments = tibble::tibble(
#'         study = "Study1", experiment = "Experiment1",
#'         design_description = "Experiment Design",
#'         library_selection = "RANDOM", instrument_model_id = 1L,
#'         library_layout = "SINGLE", library_strategy = "WGS",
#'         library_source = "GENOMIC"
#'     ),
#'     runs = tibble::tibble(
#'         run = "Run1", experiment = "Experiment1", run_file_type = "srf",
#'         alias = "Sample1", files = list("raw.fastq.gz.c4gh")
#'     ),
#'     datasets = tibble::tibble(
#'         dataset = "Dataset1", title = "Dataset Title",
#'         description = "Dataset Description",
#'         policy_accession_id = "EGAP00000000001",
#'         dataset_types = list("Whole genome sequencing"),
#'         runs = list("Run1")
#'     )
#' )
#'
#' default_validator(minimal_metadata)
#'
#' @export
default_validator <- function(meta, aliases = NULL) {
    # If aliases are not defined as a function argument, try to get them from
    # the metadata object
    if (is.null(aliases)) {
        if ("aliases" %in% names(meta)) {
            aliases <- meta$aliases
        } else {
            stop("Aliases are not specified and not available in metadata.")
        }
    }

    validations <- list(
        .basic_validator(meta, aliases, "study", "studies"),
        .basic_validator(meta, aliases, "experiment", "experiments"),
        .basic_validator(meta, aliases, "alias", "samples"),
        .basic_validator(meta, aliases, "run", "runs"),
        .basic_validator(meta, aliases, "dataset", "datasets"),
        .basic_validator(meta, aliases, "analysis", "analyses"),
        .submission_validator(meta, aliases),
        .runs_extra_validator(meta, aliases),
        .studies_extra_validator(meta, aliases),
        .datasets_extra_validator(meta, aliases),
        .analyses_extra_validator(meta, aliases),
        .dataset_analysis_validator(meta, aliases)
    )

    vs <- do.call(
        rbind,
        lapply(Filter(Negate(is.null), validations), validate::summary)
    )

    .summarise_validation(vs)
    return(vs)
}

#' Summarize Validation Results
#'
#' Aggregates and summarizes the results of multiple validation checks,
#' providing counts of failures, missing values, errors, and warnings.
#'
#' @param validation_summary A data frame of validation object summaries created
#'   using the \code{validate} package, one per row.
#'
#' @return Prints a summary message indicating the number of validation issues
#'   found, or confirms that validation passed successfully.
#'
#' @examples
#' v <- validate::validator(x > 0)
#' data <- data.frame(x = c(1, -1, 3))
#' validation_result <- validate::summary(validate::confront(data, v))
#' Rega:::.summarise_validation(validation_result)
#'
#' @keywords internal
.summarise_validation <- function(validation_summary) {
    required_cols <- c("fails", "nNA", "error", "warning")
    if (!all(required_cols %in% names(validation_summary))) {
        stop("Missing required columns in 'validation_summary'.")
    }

    totals <- c(
        fails = sum(validation_summary$fails > 0),
        nas = sum(validation_summary$nNA > 0),
        error = sum(validation_summary$error),
        warning = sum(validation_summary$warning)
    )

    if (any(totals > 0)) {
        err_msg <- sprintf(
            paste0(
                "Validation failed! See validation object for details:\n",
                "Fails: %s\nNAs: %s\nErrors: %s\nWarnings %s"
            ),
            totals["fails"], totals["nas"], totals["error"], totals["warning"]
        )
        message(err_msg)
    } else {
        message("Validation passed!")
    }
}

#' Create a Basic Validator for Data Columns
#'
#' Generates a validator to check multiple conditions on a specified data
#' column. Following validations are performed:
#'  - are all entries specified (non-NA)
#'  - are all entries unique
#'  - are all entries present in aliases
#'  - are all aliases present in the sheet
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   sheets, which contain columns to be validated.
#' @param aliases A named list of reference values used for validation.
#' @param column A string specifying the name of the column to validate.
#' @param code_list_entry A string specifying the alias entry in \code{aliases}
#'   and sheet name in metadata to compare against.
#'
#' @return A validation object containing the results of the check or NULL if
#'  sheet specified by \code{ode_list_entry} was not present in metadata
#'
#' @importFrom validate validator confront is_unique
#'
#' @examples
#' meta <- list(sample_codes = data.frame(sample_ids = c("A", "B", "C")))
#' aliases <- list(sample_codes = c("A", "B", "C", "D"))
#' Rega:::.basic_validator(meta, aliases, "sample_ids", "sample_codes")
#'
#' @keywords internal
.basic_validator <- function(meta, aliases, column, code_list_entry) {
    template <- c(
        "!is.na({column})",
        "is_unique({column})",
        "{column} %in% aliases[['{code_list_entry}']]",
        "aliases[['{code_list_entry}']] %in% {column}"
    )

    names(template) <- paste0(
        column,
        "_",
        c("is_na", "is_unique", "in_aliases", "all_aliases")
    )

    template_df <- data.frame(rule = template)
    template_df$name <- rownames(template_df)
    template_df$description <- ""
    template_df$rule <- gsub(
        "\\{column\\}", column, template_df$rule
    )
    template_df$rule <- gsub(
        "\\{code_list_entry\\}", code_list_entry, template_df$rule
    )

    cond <- validate::validator(.data = template_df)

    v <- if (code_list_entry %in% names(meta)) {
        confront(meta[[code_list_entry]], cond, ref = list(aliases = aliases))
    } else {
        NULL
    }

    return(v)
}

#' Validate Submission Metadata
#'
#' Validates the metadata by checking the following:
#' - submission title is specified
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation (not
#'   used in this function but included for consistency with other validators).
#'
#' @return A validation object containing the results of the check.
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(submission = data.frame(title = c("Submission A", NA)))
#' Rega:::.submission_validator(meta, list())
#'
#' @keywords internal
.submission_validator <- function(meta, aliases) {
    title <- NULL # nolint
    cond <- validate::validator(
        submission_title_is_na = !is.na(title)
    )
    v <- confront(meta$submission, cond)
    return(v)
}

#' Validate Studies Metadata
#'
#' Validates the metadata by checking the following:
#' - study titles are unique
#' - study descriptions are unique
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation (not
#'   used in this function but included for consistency with other validators).
#'
#' @return A validation object containing the results of the check.
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(studies = data.frame(
#'     title = c("StudyA", "StudyA", "StudyB"),
#'     description = c("Desc1", "Desc2", NA)
#' ))
#' Rega:::.studies_extra_validator(meta, list())
#'
#' @keywords internal
.studies_extra_validator <- function(meta, aliases) {
    title <- description <- NULL # nolint
    cond <- validate::validator(
        studies_title_is_unique = is_unique(title),
        studies_description_is_unique = is_unique(description)
    )
    v <- confront(meta$studies, cond, ref = list(aliases = aliases))
    return(v)
}

#' Validate Runs Metadata
#'
#' Validates the metadata by checking the following:
#' - experiments for runs are specified
#' - samples for runs are specified
#' - file types for runs are specified
#' - files for runs are specified
#' - experiments for runs are present in aliases
#' - samples for runs are present in aliases
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation.
#'
#' @return A validation object containing the results of the check.
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(runs = data.frame(
#'     experiment = c("ExperimentA", "ExperimentB", NA),
#'     alias = c("Sample1", "Sample2", "Sample1"),
#'     run_file_type = c("fastq", NA, "fastq"),
#'     files = I(list(c("File1", "File2"), c("File3"), NA))
#' ))
#'
#' aliases <- list(
#'     experiments = c("ExperimentA", "ExperimentB", "ExperimentC"),
#'     samples = c("Sample1", "Sample3")
#' )
#'
#' validate::summary(Rega:::.runs_extra_validator(meta, aliases))
#'
#' @keywords internal
.runs_extra_validator <- function(meta, aliases) {
    experiment <- alias <- run_file_type <- files <- NULL # nolint
    cond <- validate::validator(
        run_experiment_is_na = !is.na(experiment),
        run_sample_is_na = !is.na(alias),
        run_file_type_is_na = !is.na(run_file_type),
        run_file_is_na = !is.na(files),
        run_file_is_unique = is_unique(unlist(files)),
        run_experiment_in_aliases = experiment %in% aliases$experiments,
        run_sample_in_aliases = alias %in% aliases$samples
    )
    v <- confront(meta$runs, cond, ref = list(aliases = aliases))
    return(v)
}

#' Validate Analyses Metadata
#'
#' Validates the metadata by checking the following:
#' - analyses titles are unique
#' - analyses descriptions are unique
#' - files for analyses are specified
#' - experiments for analyses are present in aliases
#' - samples for analyses are present in aliases
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation.
#'
#' @return A validation object containing the results of the check or NULL if
#'   analyses sheet was not present in metadata
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(analyses = data.frame(
#'     title = c("TitleA", "TitleB", NA),
#'     description = c("DescriptionA", "DescriptionB", "DescriptionA"),
#'     samples = I(
#'         list(c("Sample1", NA), c("Sample2", "Sample3"), c("Sample3"))
#'     ),
#'     experiments = I(list(NA, c("ExpA", "ExpB"), c("ExpD"))),
#'     files = I(list(c("File1", "File2"), c("File3"), NA))
#' ))
#'
#' aliases <- list(
#'     samples = c("Sample1", "Sample3"),
#'     experiments = c("ExpA", "ExpB", "ExpC")
#' )
#'
#' validate::summary(Rega:::.analyses_extra_validator(meta, aliases))
#'
#' @keywords internal
.analyses_extra_validator <- function(meta, aliases) {
    title <- description <- samples <- experiments <- files <- NULL # nolint
    cond <- validate::validator(
        analysis_title_is_unique = is_unique(title),
        analysis_description_is_unique = is_unique(description),
        analysis_sample_in_aliases = unlist(samples) %in% aliases$samples,
        analysis_experiment_in_aliases =
            unlist(experiments) %in% aliases$experiments,
        analysis_file_is_na = !is.na(files),
        analysis_file_is_unique = is_unique(unlist(files))
    )
    v <- if ("analyses" %in% names(meta)) {
        confront(meta$analyses, cond, ref = list(aliases = aliases))
    } else {
        NULL
    }
    return(v)
}

#' Validate Datasets Metadata
#'
#' Validates the metadata by checking the following:
#' - datasets titles are unique
#' - datasets descriptions are unique
#' - runs for datasets are present in aliases (as nested lists)
#' - all runs in aliases are present in datasets (as nested lists)
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation (not
#'   used in this function but included for consistency with other validators).
#'
#' @return A validation object containing the results of the check.
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(datasets = data.frame(
#'     title = c("TitleA", "TitleB", NA),
#'     description = c("DescriptionA", "DescriptionB", "DescriptionA"),
#'     runs = I(list(c("Run1", NA), c("Run2", "Run3"), c("Run3")))
#' ))
#'
#' aliases <- list(
#'     runs = c("Run1", "Run3", "Run4")
#' )
#'
#' validate::summary(Rega:::.datasets_extra_validator(meta, aliases))
#'
#' @keywords internal
.datasets_extra_validator <- function(meta, aliases) {
    title <- description <- runs <- NULL # nolint
    cond <- validate::validator(
        dataset_title_is_unique = is_unique(title),
        dataset_description_is_unique = is_unique(description),
        dataset_run_in_aliases = unlist(runs) %in% aliases$runs,
        dataset_all_aliases_in_run = aliases$runs %in% unlist(runs)
    )
    v <- confront(meta$datasets, cond, ref = list(aliases = aliases))
}

#' Validate Datasets Analyses Metadata
#'
#' Validates the metadata by checking the following:
#' - analyses for datasets are present in aliases
#'
#' @param meta A list containing parsed EGA metadata, where elements correspond
#'   to individual sheets.
#' @param aliases A list of aliases, reference values used for validation (not
#'   used in this function but included for consistency with other validators).
#'
#' @return A validation object containing the results of the check or NULL if
#'   analyses sheet was not present in metadata
#'
#' @importFrom validate validator confront
#'
#' @examples
#' meta <- list(
#'     datasets = data.frame(
#'         analyses = c("AnalysisA", "AnalysisB", NA)
#'     ),
#'     analyses = data.frame(
#'         names = c("A", "B")
#'     )
#' )
#'
#' aliases <- list(
#'     analyses = c("AnalysisA", "AnalysisC")
#' )
#'
#' validate::summary(
#'     Rega:::.dataset_analysis_validator(meta, aliases)
#' )
#'
#' @keywords internal
.dataset_analysis_validator <- function(meta, aliases) {
    analyses <- NULL # nolint
    cond <- validate::validator(
        dataset_analysis_in_aliases = unlist(analyses) %in% aliases$analyses
    )
    v <- if ("analyses" %in% names(meta)) {
        confront(meta$datasets, cond, ref = list(aliases = aliases))
    } else {
        NULL
    }
    return(v)
}

# httr request payload validation  ---------------------------------------------

#' Retrieve the Schema for an API Operation
#'
#' @param op List. The API operation definition containing a `requestBody`
#' element with content and schema details.
#'
#' @return The schema for the operation's JSON request body, or `NULL` if no
#' schema is defined.
#'
#' @examples
#' # Get operations from API
#' opdefs <- extract_operation_definitions(extract_api())
#'
#' # Retrieve the schema for a specific operation
#' schema <- get_operation_schema(opdefs[["post__submissions"]])
#'
#' @export
get_operation_schema <- function(op) {
    schema <- op$requestBody$content$`application/json`$schema
    return(schema)
}

#' Validate a Payload Against a JSON Schema
#'
#' Function handles `oneOf` directives in a way that it in a case of validation
#' fail, it displays the overall result of the validation as first and then it
#' tests separately against all `oneOf` sub schemas.
#'
#' @param payload JSON string or single row of data frame converted to JSON
#'   representation with `unbox_row` function or a list with all items of length
#'   1 converted to JSON representation with `unbox_list` function. The payload
#'   to validate against the schema.
#' @param schema List. The JSON schema defining the validation rules.
#'
#' @return Logical value indicating whether the payload is valid. If invalid,
#'   the result includes an `errors` attribute detailing the validation errors.
#'
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_validator
#'
#' @examples
#' schema <- list(
#'     type = "object",
#'     properties = list(
#'         id = list(type = "integer"),
#'         title = list(type = "string")
#'     ),
#'     required = c("id")
#' )
#'
#' payload_true <- data.frame(id = c(12345), title = c("abcd"))
#' payload_false <- data.frame(id = c("12345"), title = c(0.355))
#'
#' validate_schema(jsonlite::unbox(payload_true), schema)
#' validate_schema(jsonlite::unbox(payload_false), schema)
#'
#' @export
validate_schema <- function(payload, schema) {
    if (!is.list(schema)) stop("'schema' must be a list.")
    json_schema <- toJSON(schema) # Convert to json

    # Assume it's a json formatted string otherwise
    if (!is.character(payload)) {
        if (!identical(class(payload), c("scalar", "data.frame"))) {
            warn_msg <- paste(
                "Payload was probably not processed with 'unbox_*' function.",
                "API might reject it."
            )
            warning(warn_msg)
        }
        payload <- toJSON(payload)
    }

    # Create validator
    topv <- json_validator(json_schema, engine = "imjv")

    valid <- topv(payload, verbose = TRUE)

    # If its oneOf schema, we will do validation of individual entries to see
    # what is missing
    is_oneof_schema <- ifelse("oneOf" %in% names(schema), TRUE, FALSE)

    if (!valid && is_oneof_schema) {
        # Iterate through list of oneOf schemas
        individual_validations <- lapply(schema$oneOf, function(x) {
            # Create validator and validate
            subv <- json_validator(toJSON(x), engine = "imjv")

            subv(payload, verbose = TRUE)
        })

        # Create a single data frame of errors
        individual_errors <- do.call(
            rbind,
            lapply(individual_validations, \(x) if (!x) attributes(x)$errors)
        )

        # Add them to the result of original validator
        attr(valid, "errors") <- rbind(
            attributes(valid)$errors,
            individual_errors
        )
    }

    return(valid)
}


#' Convert Validation Results to a Message
#'
#' @param v Logical. The validation result, which may include an
#' `errors` attribute detailing validation errors.
#'
#' @return A character string summarizing the validation results. If validation
#' errors are present, they are included in the message; otherwise, a success
#' message is returned.
#'
#' @importFrom utils capture.output
#'
#' @examples
#' validation_result <- FALSE
#' attr(validation_result, "errors") <- data.frame(
#'     field = c("name"),
#'     message = c("Missing")
#' )
#' msg <- validation_to_msg(validation_result)
#' message(msg)
#'
#' @export
validation_to_msg <- function(v) {
    if (!is.logical(v)) stop("Validation result must be 'logical'.")

    # if (!v && is.null(attributes(v))) {
    #     stop("Validation failed, but not error attributes are present.")
    # }
    #
    # if (v && "errors" %in% names(attributes(v))) {
    #     stop("Validation succeeded, but error attributes are present.")
    # }

    if (is.null(attributes(v))) {
        val_msg <- "No validation errors found."
    } else {
        # Convert the data frame to a formatted string
        val_msg <- paste0(
            "Request body raised following validation errors:\n\n",
            paste0(
                capture.output(
                    format(attributes(v)$errors, row.names = FALSE)
                ),
                collapse = "\n"
            )
        )
    }

    return(val_msg)
}
