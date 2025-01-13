#' Submit New Data to EGA
#'
#' This function creates a new submission and associates all specified data with
#' it. Following data has to be present in the request data object: submission
#' studies, experiments, samples, runs, analyses, datasets. The files associated
#' with the submission must be present in the EGA Inbox and they are fetched and
#' matched according to Inbox path. In case the submission is interrupted or
#' fails, all the information entered into EGA database is rolled back apart
#' from the submission itself. If the workflow successfully creates a submission,
#' but fails in the following steps, the returned submission ID can be used as
#' a parameter to the workflow to continue entering data into existing
#' submission.
#' If logfile is specified, the responses from successfully executed steps
#' (even if the error occurs), will be saved.
#'
#' @param request_data List of data frames. Parsed submission metadata containing
#' correctly formatted and linked information for submission
#' @param client List of functions. EGA API client created by `create_client`
#' function from EGA API schema.
#' @param logfile Character. Path of log file to log the `httr2` responses from
#' individual operations or `NULL`. Defaults to `NULL`.
#' @param id Integer.
#' @param retrieve_if_exists Logical.
#' @param ... List. Additional arguments to the function.
#'
#' @return List of data frames. Parsed response objects from httr2 requests
#'
#' @importFrom rlang !!!
#'
#' @export
new_submission <- function(request_data, client, logfile = NULL, id = NULL,
                           retrieve_if_exists = FALSE, ...) {
  luts <- list()
  responses <- list()

  if (!is.logical(retrieve_if_exists)) {
    message("'retrieve_if_exists' needs to be logical. Setting to FALSE.")
    retrieve_if_exists <- FALSE
  }

  all_steps <- c(
    "files", "analysis_files", "submission", "studies", "experiments",
    "samples", "runs", "analyses", "datasets"
  )
  sm <- step_msg(length(intersect(all_steps, names(request_data))))

  # 1. Files -----
  # Get file provisional IDs based on file names/paths
  # Retrieve and parse one at a time, there is 500 query limit
  # Create look-up based on retrieved provisional IDs and metadata
  sm("Retrieving Raw Files")

  responses$raw_files <- do.call(
    rbind,
    lapply(
      unlist(request_data$runs$files),
      \(x) client$get__files(prefix = x)
    )
  )

  # Stop if some files are not present in EGA Inbox
  stopifnot(
    nrow(responses$raw_files) == length(unlist(request_data$runs$files))
  )

  luts$raw_files <- setNames(
    responses$raw_files$provisional_id,
    unlist(request_data$runs$files)
  )

  # 2. Submission -----
  # Create submission and store the provisional ID that will be used throughout
  # the workflow
  tryCatch({
    withCallingHandlers(
      {
        sm("Creating Submission")
        # If submission ID was specified in dots use it, otherwise create new
        # based on the request data
        if (!is.null(id)) {
          submission_id <- id
          responses$submission <- client$get__submissions__provisional_id(submission_id)
        } else {
          responses$submission <- client$post__submissions(
            body = unbox_row(request_data$submission[1, ])
          )
          submission_id <- responses$submission$provisional_id[1]
        }
      },
      error = workflow_error_handler(
        "submission",
        responses,
        logfile
      )
    )
  })

  # 3. Samples ------
  # Submit samples, they have no dependencies on other tables
  tryCatch({
    withCallingHandlers(
      {
        sm("Adding Samples")

        # Submit table
        responses$samples <- get_or_post(
          submission_id,
          request_data$samples,
          client,
          "samples",
          retrieve_if_exists = retrieve_if_exists
        )

        luts$samples <- setNames(
          responses$samples$provisional_id,
          request_data$samples$alias
        )
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__samples(submission_id)
      )
    )
  })

  # 4. Studies ------
  # Submit studies, they have no dependencies on other tables
  tryCatch({
    withCallingHandlers(
      {
        sm("Adding Studies")

        # Submit table
        responses$studies <- get_or_post(
          submission_id,
          request_data$studies,
          client,
          "studies",
          retrieve_if_exists = retrieve_if_exists
        )

        # Create LUT
        luts$studies <- setNames(
          responses$studies$provisional_id,
          request_data$studies$study
        )
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__studies(submission_id)
      )
    )
  })

  # 5. Experiments ------
  # Merge studies lookup table with experiments
  tryCatch({
    withCallingHandlers(
      {
        sm("Adding Experiments")

        # Replace study IDs
        request_data$experiments <- lut_add(
          request_data$experiments,
          "study_provisional_id",
          "study",
          luts$studies
        )

        # Submit table
        responses$experiments <- get_or_post(
          submission_id,
          request_data$experiments,
          client,
          "experiments",
          retrieve_if_exists = retrieve_if_exists
        )

        # Create LUT
        luts$experiments <- setNames(
          responses$experiments$provisional_id,
          request_data$experiments$experiment
        )
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__experiments(submission_id)
      )
    )
  })

  # 6. Runs ------
  tryCatch({
    withCallingHandlers(
      {
        sm("Adding Runs")

        # Replace IDs
        multi_lut_args <- list(
          list("experiment_provisional_id", "experiment", luts$experiments),
          list("sample_provisional_id", "alias", luts$samples),
          list("files", "files", luts$raw_files)
        )

        request_data$runs <- multi_lut_add(request_data$runs, !!!multi_lut_args)

        # Submit table
        responses$runs <- get_or_post(
          submission_id,
          request_data$runs,
          client,
          "runs",
          retrieve_if_exists = retrieve_if_exists
        )

        # Create LUT
        luts$runs <- setNames(
          responses$runs$provisional_id,
          request_data$runs$run
        )
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__runs(submission_id)
      )
    )
  })

  # 7. Analyses ------
  # Submit analyses
  # This is the only optional step in the analysis
  # analysis and analyses files sheet are removed during the parsing if the
  # analysis is not specified
  tryCatch({
    withCallingHandlers(
      {
        # If no analyses are present, return NULL
        if ("analyses" %in% names(request_data) &&
          "analysis_files" %in% names(request_data)
        ) {
          sm("Retrieving Analysis Files")
          responses$analysis_files <- do.call(
            rbind,
            lapply(
              unlist(request_data$analyses$files),
              \(x) client$get__files(prefix = x)
            )
          )

          # Stop if some files are not present in EGA Inbox
          stopifnot(
            nrow(responses$analysis_files) == length(unlist(request_data$analyses$files))
          )

          # Create LUT
          luts$analysis_files <- setNames(
            responses$analysis_files$provisional_id,
            unlist(request_data$analyses$files)
          )

          sm("Adding Analyses")

          # Replace IDs
          multi_lut_args <- list(
            list("study_provisional_id", "study", luts$studies),
            list("experiment_provisional_ids", "experiments", luts$experiments),
            list("sample_provisional_ids", "samples", luts$sample),
            list("files", "files", luts$analysis_files)
          )

          request_data$analyses <- multi_lut_add(
            request_data$analyses,
            !!!multi_lut_args
          )

          # Submit table
          responses$analyses <- get_or_post(
            submission_id,
            request_data$analyses,
            client,
            "analyses",
            retrieve_if_exists = retrieve_if_exists
          )

          # Create LUT
          luts$analyses <- setNames(
            responses$analyses$provisional_id,
            request_data$analyses$analysis
          )

          # Replace analysis IDs in datasets
          request_data$datasets <- lut_add(
            request_data$datasets,
            "analysis_provisional_ids",
            "analyses",
            luts$analyses
          )
        }
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__analyses(submission_id)
      )
    )
  })

  # 8. Datasets ------
  # Submit datasets
  tryCatch({
    withCallingHandlers(
      {
        sm("Adding Datasets")

        # Replace run IDs
        request_data$datasets <- lut_add(
          request_data$datasets,
          "run_provisional_ids",
          "runs",
          luts$runs
        )

        # Submit table
        responses$datasets <- get_or_post(
          submission_id,
          request_data$datasets,
          client,
          "datasets",
          retrieve_if_exists = retrieve_if_exists
        )
      },
      error = workflow_error_handler(
        "samples",
        responses,
        logfile,
        client$delete__submissions__provisional_id__datasets(submission_id)
      )
    )
  })

  save_log(responses, logfile)

  return(responses)
}

#' Retrieve or Delete Submission Data
#'
#' Handles retrieval or deletion of data associated with a submission
#' accession/provisional ID using a specified client and method.
#'
#' @param id Character or numeric. Represents the submission identifier.
#' Can be either an accession or provisional ID.
#' @param client An API client object with \code{get} and \code{delete} methods.
#' @param method A string specifying the operation to perform. Valid options
#' are "get" or "delete".
#'
#' @return A named list containing responses for datasets, analyses, runs,
#' experiments, samples, and studies.
#'
#' @examples
#' mock_client <- list(
#'   "get__submissions__accession_id__datasets" = function(id) list(data = id),
#'   "delete__submissions__provisional_id__datasets" = function(id) list(status = "deleted")
#' )
#' use_submission("EGAB12345678901", mock_client, "get")
#'
#' @export
use_submission <- function(id, client, method) {
  if (is_accession(id)) {
    base_url <- "submissions__accession_id"
  } else {
    base_url <- "submissions__provisional_id"
  }

  # List endpoints from the last since the earlier ones depend on them and
  # wouldn't be deleted otherwise
  # submission endpoint itself is omitted so the same function could be used
  # to retrieve and delete contents
  all_ops <- c(
    "datasets", "analyses", "runs", "experiments", "samples", "studies"
  )

  method <- tolower(method)

  if (!method %in% c("get", "delete")) {
    stop("Only 'get' and 'delete' methods are currently supported.")
  }

  resp <- lapply(all_ops, function(x) {
    s <- paste(method, "__", base_url, "__", x, sep = "")
    if (s %in% names(client)) client[[s]](id)
  })

  resp <- setNames(resp, all_ops)
  return(resp)
}

#' Retrieve Submission Data and Log Responses
#'
#' Retrieves data associated with a submission ID using the client and logs the
#' responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Can be either an
#' accession or provisional ID.
#' @param client An API client object with \code{get} methods.
#' @param logfile A string specifying the path to a log file. If \code{NULL},
#'   no log is written.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses including submission data and associated
#' datasets, analyses, runs, experiments, samples, and studies.
#'
#' @examples
#' mock_client <- list(
#'   "get__submissions__accession_id" = function(id) list(data = id),
#'   "get__submissions__accession_id__datasets" = function(id) list(datasets = id)
#' )
#' get_submission("EGAB12345678901", mock_client)
#'
#' @export
get_submission <- function(id, client, logfile = NULL, ...) {
  if (is_accession(id)) {
    base_url <- "submissions__accession_id"
  } else {
    base_url <- "submissions__provisional_id"
  }

  responses <- c(
    # Include submission endpoint for GET method
    list(submission = client[[paste0("get", "__", base_url)]](id)),
    use_submission(id, client, "get")
  )

  save_log(responses, logfile)

  return(responses)
}

#' Delete Submission Contents and Log Responses
#'
#' Deletes all data associated with a submission ID using the client and logs
#' the responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Can be either an
#' accession or provisional ID.
#' @param client An API client object with \code{delete} methods.
#' @param logfile A string specifying the path to a log file. If \code{NULL},
#' no log is written.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses for the deletion of associated datasets,
#'   analyses, runs, experiments, samples, and studies.
#'
#' @examples
#' mock_client <- list(
#'   "delete__submissions__provisional_id__datasets" = function(id) list(status = "deleted")
#' )
#' delete_submission_contents(5678901, mock_client)
#'
#' @export
delete_submission_contents <- function(id, client, logfile = NULL, ...) {
  responses <- use_submission(id, client, "delete")
  save_log(responses, logfile)
  return(responses)
}

#' Delete a Submission and Log Responses
#'
#' Deletes a submission identified by its ID using the client and logs the
#' response if a logfile is specified.
#'
#' @param id A string representing the submission identifier (provisional ID).
#' @param client An API client object with a \code{delete} method for
#' submissions.
#' @param logfile A string specifying the path to a log file. If \code{NULL},
#' no log is written.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list containing the response for the submission deletion.
#'
#' @examples
#' mock_client <- list(
#'   delete__submissions__provisional_id = function(id) list(status = "deleted")
#' )
#' delete_submission("5678901", mock_client)
#'
#' @export
delete_submission <- function(id, client, logfile = NULL, ...) {
  responses <- list(
    submission = client$delete__submissions__provisional_id(id)
  )
  save_log(responses, logfile)
  return(responses)
}

#' Rollback Submission Endpoints and Log Responses
#'
#' Rolls back specified endpoints for a submission identified by its ID using
#' the client and logs the responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Must be an
#' accession ID.
#' @param client An API client object with \code{put} methods and rollback
#' operations.
#' @param endpoints A character vector of endpoint names to rollback.
#' @param logfile A string specifying the path to a log file. If \code{NULL},
#' no log is written.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses from the rollback operations for each endpoint.
#'
#' @examples
#' mock_client <- list(
#'   "put__submissions__accession_id__datasets_rollback" = function(id) list(status = "rolled back")
#' )
#' rollback_submission("provisional123", mock_client, c("datasets"))
#'
#' @export
rollback_submission <- function(id, client, endpoints, logfile = NULL, ...) {
  if (is_accession(id)) {
    stop(
      "Incorrect format of accesssion ID.
       Following format is required: ^EGA\\d{11}$"
    )
  }

  responses <- lapply(endpoints, function(x) {
    endpoint_str <- paste0("put__submissions__accession_id__", x, "_rollback")
    client[[endpoint_str]](id)
  })

  save_log(responses, logfile)
  return(responses)
}


# put_submission <- function(request_data, client, logfile = NULL, ...) {
#   responses <- list()
#   # base_url = "put__"
#   all_endpoints <- c(
#     "submission", "studies", "experiments", "samples", "runs", "analyses", "datasets"
#   )
#
#   resp = lapply(all_endpoints, function(x) {
#     if(x %in% names(request_data)) {
#       if(lengt())
#       resp = client[[paste0("put__", x)]](body = toJSON(request_data))
#     }
#   })
#
#
#
#   # TODO fill in
#
#   save_log(responses, logfile)
#
#   return(responses)
# }
