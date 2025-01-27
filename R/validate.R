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
#' @importFrom validate validator confront is_unique summary
#'
#'
#'
#' @export
default_validator <- function(meta, aliases = NULL) {
  # lint
  alias <- samples <- study <- experiment <- experiments <- NULL # nolint
  run <- runs <- analyses <- NULL # nolint
  files <- run_file_type <- title <- description <- resp <- NULL # nolint

  # If aliases are not defined as a function argument, try to get them from
  # the metadata object
  if (is.null(aliases)) {
    if ("aliases" %in% names(meta)) {
      aliases <- meta$aliases
    } else {
      stop("Aliases are not specified and not available in metadata.")
    }
  }

  # Convenience function to create a basic validator that would be applied in
  # the same manner to multiple columns. Currently handles:
  # - are all entries specified (non-NA)
  # - are all entries unique
  # - are all entries present in aliases
  # - all all aliases present in the dataset
  create_basic_validator <- function(column, code_list_entry) {
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

    v <- validate::validator(.data = template_df)
    return(v)
  }

  submission_validator <- validate::validator(
    submission_title_is_na = !is.na(title)
  )
  # Extra validation for study titles and descriptions
  studies_extra_validator <- validate::validator(
    studies_title_is_unique = is_unique(title),
    studies_description_is_unique = is_unique(description)
  )

  experiments_extra_validator <- validate::validator(
    study_in_aliases = study %in% aliases$studies
  )

  # Runs table is slightly more complicated, so more checks are done with
  # samples and files
  runs_extra_validator <- validate::validator(
    run_experiment_is_na = !is.na(experiment),
    run_sample_is_na = !is.na(alias),
    run_file_type_is_na = !is.na(run_file_type),
    run_file_is_na = !is.na(files),
    run_experiment_in_aliases = experiment %in% aliases$experiments,
    run_sample_in_aliases = alias %in% aliases$samples
  )

  # Extra validation for analyses
  # - unique titles and descriptions
  # - samples present
  # - experiment present
  # - files present
  analyses_extra_validator <- validate::validator(
    analysis_title_is_unique = is_unique(title),
    analysis_description_is_unique = is_unique(description),
    analysis_sample_in_aliases = unlist(samples) %in% aliases$samples,
    analysis_experiment_in_aliases =
      unlist(experiments) %in% aliases$experiments,
    analysis_file_is_na = !is.na(files)
  )

  # Extra validation for datasets
  # - unique titles and descriptions
  # - runs are stored as nested lists
  datasets_extra_validator <- validate::validator(
    dataset_title_is_unique = is_unique(title),
    dataset_description_is_unique = is_unique(description),
    dataset_run_in_aliases = unlist(runs) %in% aliases$runs,
    dataset_all_aliases_in_run = aliases$runs %in% unlist(runs)
  )

  dataset_analysis_validator <- validate::validator(
    dataset_analysis_in_aliases = unlist(analyses) %in% aliases$analyses
  )

  # NOTE One submission was rejected because the study title and dataset
  # TODO
  # title were identical.
  cross_validator <- validator()

  validations <- list(
    confront(
      meta$studies,
      create_basic_validator("study", "studies"),
      ref = list(aliases = aliases)
    ),
    confront(
      meta$experiments,
      create_basic_validator("experiment", "experiments"),
      ref = list(aliases = aliases)
    ),
    confront(
      meta$samples,
      create_basic_validator("alias", "samples"),
      ref = list(aliases = aliases)
    ),
    confront(
      meta$runs,
      create_basic_validator("run", "runs"),
      ref = list(aliases = aliases)
    ),
    confront(
      meta$datasets, create_basic_validator("dataset", "datasets"),
      ref = list(aliases = aliases)
    ),
    confront(meta$submission, submission_validator),
    confront(meta$runs, runs_extra_validator, ref = list(aliases = aliases)),
    confront(
      meta$studies, studies_extra_validator,
      ref = list(aliases = aliases)
    ),
    confront(
      meta$datasets, datasets_extra_validator,
      ref = list(aliases = aliases)
    )
  )

  if ("analyses" %in% names(meta)) {
    validations <- c(
      validations,
      list(
        confront(
          meta$analyses, create_basic_validator("analysis", "analyses"),
          ref = list(aliases = aliases)
        ),
        confront(
          meta$analyses, analyses_extra_validator,
          ref = list(aliases = aliases)
        ),
        confront(
          meta$datasets, dataset_analysis_validator,
          ref = list(aliases = aliases)
        )
      )
    )
  }

  vs <- do.call(rbind, lapply(validations, validate::summary))

  totals <- c(
    fails = sum(vs$fails > 0),
    nNA = sum(vs$nNA > 0),
    error = sum(vs$error),
    warning = sum(vs$warning)
  )

  if (any(totals > 0)) {
    err_msg <- sprintf(
      paste0(
        "Validation failed! See validation object for details:\n",
        "Fails: %s\nnNA: %s\nErrors: %s\nWarnings %s"
      ),
      totals["fails"], totals["nNA"], totals["error"], totals["warning"]
    )
    message(err_msg)
  } else {
    message("Validation passed!")
  }

  return(vs)
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
#' @param payload List or JSON string. The payload to validate against the
#' schema.
#' @param schema List. The JSON schema defining the validation rules.
#'
#' @return Logical value indicating whether the payload is valid. If invalid,
#' the result includes an `errors` attribute detailing the validation errors.
#'
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_validator
#'
#' @examples
#' schema <- list(
#'   type = "object",
#'   properties = list(
#'     id = list(type = "integer"),
#'     title = list(type = "string")
#'   ),
#'   required = c("id")
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
  # Convert to json
  json_schema <- toJSON(schema)

  # Assume it's a json formatted string otherwise
  if (!is.character(payload)) {
    payload <- toJSON(payload)
  }

  # Create validator
  topv <- json_validator(json_schema, engine = "imjv")


  valid <- topv(payload, verbose = TRUE)

  # If its oneOf schema, we will do validation of individual entries to see what
  # is missing
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
    attr(valid, "errors") <- rbind(attributes(valid)$errors, individual_errors)
  }

  return(valid)
}


#' Convert Validation Results to a Message
#'
#' @param v Logical or list. The validation result, which may include an
#' `errors` attribute detailing validation errors.
#'
#' @return A character string summarizing the validation results. If validation
#' errors are present, they are included in the message; otherwise, a success
#' message is returned.
#'
#' @importFrom utils capture.output
#'
#' @examples
#' # Generate a validation message
#' validation_result <- list(
#'   errors = data.frame(field = "name", error = "Missing")
#' )
#' msg <- validation_to_msg(validation_result)
#' message(msg)
#'
#' @export
validation_to_msg <- function(v) {
  # TODO fix examples
  if (is.logical(v)) v <- attributes(v)

  if (is.null(v)) {
    val_message <- "No validation errors found."
  } else {
    # Convert the data frame to a formatted string
    val_message <-
      val_message <- paste0(
        "Request body raised following validation errors:\n\n",
        paste0(
          # capture.output(print(v$errors, row.names = FALSE)),
          capture.output(format(v$errors, row.names = FALSE)),
          collapse = "\n"
        )
      )
  }

  return(val_message)
}
