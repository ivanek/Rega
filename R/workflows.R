#' Submit New Data to EGA
#'
#' This function creates a new submission and associates all specified data with
#' it. Following data has to be present in the request data object: submission
#' studies, experiments, samples, runs, analyses, datasets. The files associated
#' with the submission must be present in the EGA Inbox and they are fetched and
#' matched according to Inbox path. In case the submission is interrupted or
#' fails, all the information entered into EGA database is rolled back apart
#' from the submission itself. If the workflow successfully creates a
#' submission, but fails in the following steps, the returned submission ID can
#' be used as a parameter to the workflow to continue entering data into
#' existing submission. If logfile is specified, the responses from successfully
#' executed steps (even if the error occurs), will be saved.
#'
#' @param dat List of data frames. Parsed submission metadata
#'   containing correctly formatted and linked information for submission
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema. If \code{NULL}, default client will be
#'   created by \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param logfile Character. Path of log file to log the `httr2` responses from
#'   individual operations or \code{NULL}. Defaults to \code{NULL}.
#' @param submission_id Integer.
#' @param retrieve Logical.
#' @param ... List. Additional arguments to the function.
#'
#' @return List of data frames. Parsed response objects from httr2 requests
#'
#' @importFrom rlang !!!
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
#' ega <- create_client(extract_api(), verbosity = 0)
#'
#' # Requires credentials
#' try(
#'     new_submission(minimal_metadata, ega)
#' )
#'
#' @export
new_submission <- function(
    dat, client = NULL, logfile = NULL, submission_id = NULL,
    retrieve = FALSE, ...
) {
    # The rest of arguments are validated in the respective functions
    if (!is.list(dat) || is.null(names(dat))) {
        stop("'dat' must be a named list.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    .validate_logical_scalar(retrieve)

    # 0. Setup ---
    all_steps <- c(
        "files", "analysis_files", "submission", "studies", "experiments",
        "samples", "runs", "analyses", "datasets"
    )
    sm <- step_msg(length(intersect(all_steps, names(dat))))

    luts <- list()
    resp <- list()

    # 1. Pre-flight checks ---
    # Samples aliases must be unique per user. Are they already present in EGA?
    samples_in_db(dat$samples$alias, client, retrieve)

    # 2. Files ---
    sm("Retrieving Raw Files")
    files_resp <- fetch_files(dat$files$ega_file, client)
    resp$raw_files <- files_resp$response
    luts$raw_files <- files_resp$lut

    # 3. Submission (Creation) ---
    try_step(
        "submission", function() {
            sm("Creating Submission")
            if (!is.null(submission_id)) {
                resp$submission <<- client$get__submissions__provisional_id(
                    submission_id
                )
            } else {
                resp$submission <<- client$post__submissions(
                    body = unbox_row(dat$submission[1, ])
                )
            }
        },
        NULL,
        resp, logfile
    )

    id <- resp$submission$provisional_id[1]

    # 4. Entity Submission Loop ---
    ## Samples
    try_step(
        "samples", function() {
            sm("Adding Samples")
            # Submit table
            resp$samples <<- get_or_post(
                id, dat$samples, client, "samples", retrieve
            )
            # Create LUT
            luts$samples <<- setNames(
                resp$samples$provisional_id, dat$samples$alias
            )
        },
        client$delete__submissions__provisional_id__samples(id),
        resp, logfile
    )

    ## Studies
    try_step(
        "studies", function() {
            sm("Adding Studies")
            # Submit table
            resp$studies <<- get_or_post(
                id, dat$studies, client, "studies", retrieve
            )
            # Create LUT
            luts$studies <<- setNames(
                resp$studies$provisional_id, dat$studies$study
            )
        },
        client$delete__submissions__provisional_id__studies(id),
        resp, logfile
    )

    ## Experiments (Depends on Studies)
    try_step(
        "experiments", function() {
            sm("Adding Experiments")
            # Replace study IDs
            dat$experiments <<- lut_add(
                dat$experiments, "study_provisional_id", "study", luts$studies
            )
            # Submit table
            resp$experiments <<- get_or_post(
                id, dat$experiments, client, "experiments", retrieve
            )
            # Create LUT
            luts$experiments <<- setNames(
                resp$experiments$provisional_id, dat$experiments$experiment
            )
        },
        client$delete__submissions__provisional_id__experiments(id),
        resp, logfile
    )

    ## Runs (Depends on Experiments, Samples, Files)
    try_step(
        "runs", function() {
            sm("Adding Runs")
            # Replace IDs
            multi_lut_args <- list(
                list(
                    "experiment_provisional_id", "experiment", luts$experiments
                ),
                list("sample_provisional_id", "alias", luts$samples),
                list("files", "files", luts$raw_files)
            )
            dat$runs <<- multi_lut_add(dat$runs, !!!multi_lut_args)
            # Submit table
            resp$runs <<- get_or_post(id, dat$runs, client, "runs", retrieve)
            # Create LUT
            luts$runs <<- setNames(resp$runs$provisional_id, dat$runs$run)
        },
        client$delete__submissions__provisional_id__runs(id),
        resp, logfile
    )

    ## Analyses (Optional) ---
    # if ("analyses" %in% names(dat) && "analysis_files" %in% names(dat)) {
    if (.has_analyses(dat)) {
        try_step(
            "analyses", function() {
                sm("Retrieving Analysis Files")
                analysis_files_resp <- fetch_files(
                    dat$analysis_files$ega_file, client
                )
                resp$analysis_files <<- analysis_files_resp$response
                luts$analysis_files <<- analysis_files_resp$lut

                sm("Adding Analyses")
                # Replace IDs
                multi_lut_args <- list(
                    list("study_provisional_id", "study", luts$studies),
                    list(
                        "experiment_provisional_ids", "experiments",
                        luts$experiments
                    ),
                    list("sample_provisional_ids", "samples", luts$sample),
                    list("files", "files", luts$analysis_files)
                )
                dat$analyses <<- multi_lut_add(dat$analyses, !!!multi_lut_args)
                # Submit table
                resp$analyses <<- get_or_post(
                    id, dat$analyses, client, "analyses", retrieve
                )
                # Create LUT
                luts$analyses <<- setNames(
                    resp$analyses$provisional_id,
                    dat$analyses$analysis
                )
                # Replace analysis IDs in datasets
                dat$datasets <<- lut_add(
                    dat$datasets, "analysis_provisional_ids", "analyses",
                    luts$analyses
                )
            },
            client$delete__submissions__provisional_id__analyses(id),
            resp, logfile
        )
    }

    ## Datasets ---
    try_step(
        "datasets", function() {
            sm("Adding Datasets")
            # Replace run IDs
            dat$datasets <<- lut_add(
                dat$datasets, "run_provisional_ids", "runs", luts$runs
            )
            # Submit table
            resp$datasets <<- get_or_post(
                id, dat$datasets, client, "datasets", retrieve
            )
        },
        client$delete__submissions__provisional_id__datasets(id),
        resp, logfile
    )

    # 5. Finalize ---
    save_log(resp, logfile)
    resp
}

#' Finalise an EGA submission
#'
#' Submits the finalisation request for a submission identified by either an
#' accession or provisional ID. Validates the release date and sends optional
#' dataset changelogs.
#'
#' @param id Character scalar. The submission accession or provisional ID.
#' @param release_date Character scalar. Expected release date in YYYY-MM-DD
#'   format.
#' @param dataset_changelogs Data frame. Optional changelog metadata for
#'   associated datasets. If specified, the requred columns are `dataset` and
#'   `message`. Defaults to empty data.frame.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema. If \code{NULL}, default client will be
#'   created by \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param logfile Character. Path of log file to log the `httr2` responses from
#'   individual operations or \code{NULL}. Defaults to \code{NULL}.
#' @param ... List. Additional arguments to the function.
#'
#' @return The API response object from the finalisation request.
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' # Requires credentials
#' try(
#'     finalise_submission("123456", "2025-12-31")
#' )
#'
#' @export
finalise_submission <- function(
    id, release_date, dataset_changelogs = data.frame(), client = NULL,
    logfile = NULL, ...
) {
    if (is_accession(id)) {
        base_url <- "submissions__accession_id"
    } else if (is_provisional(id)) {
        base_url <- "submissions__provisional_id"
    } else {
        stop("Unknown ID type, must be valid accession of provisional ID.")
    }

    .validate_character_scalar(release_date)
    # Check if release day is proper format
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", release_date)) {
        stop("Incorrect 'release_date' format, must be YYYY-MM-DD.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    # Check if dataset_changelog has proper format
    if (!identical(dataset_changelogs, data.frame())) {
        target_cols <- c("dataset", "message")
        missing_cols <- !target_cols %in% names(dataset_changelogs)

        if (any(missing_cols)) {
            stop(sprintf(
                "'dataset_changelogs' must contain following columns: %s.",
                paste(target_cols, collapse = ", ")
            ))
        }

        is_valid_col <- vapply(dataset_changelogs[target_cols], function(x) {
            is.character(x) && !any(is.na(x))
        }, logical(1))

        if (!all(is_valid_col)) {
            stop(
                "'dataset_changelogs' columns must be character type without NA
                values."
            )
        }
    }

    body <- list(
        expected_release_date = release_date,
        dataset_changelogs = dataset_changelogs
    )

    responses <- client[[paste0("post__", base_url, "__finalise")]](
        id, toJSON(body, auto_unbox = TRUE)
    )
    save_log(responses, logfile)
    responses
}

#' Retrieve or Delete Submission Data
#'
#' Handles retrieval or deletion of data associated with a submission
#' accession/provisional ID using a specified client and method.
#'
#' @param id Character or numeric. Represents the submission identifier. Can be
#'   either an accession or provisional ID.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema with \code{get} and \code{delete} methods. If
#'   \code{NULL}, default client will be created by
#'   \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param method A string specifying the operation to perform. Valid options are
#'   "get" or "delete".
#'
#' @return A named list containing responses for datasets, analyses, runs,
#'   experiments, samples, and studies.
#'
#' @importFrom rlang is_empty
#'
#' @examples
#' mock_client <- list(
#'     "get__submissions__accession_id__datasets" = function(id) {
#'         list(data = id)
#'     },
#'     "delete__submissions__provisional_id__datasets" =
#'         function(id) list(status = "deleted")
#' )
#' use_submission("EGAB12345678901", mock_client, "get")
#'
#' @export
use_submission <- function(id, method, client = NULL) {
    .validate_character_scalar(method)

    if (is_accession(id)) {
        base_url <- "submissions__accession_id"
    } else if (is_provisional(id)) {
        base_url <- "submissions__provisional_id"
    } else {
        stop("Unknown ID type, must be valid accession of provisional ID.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
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
    resp
}

#' Retrieve Submission Data and Log Responses
#'
#' Retrieves data associated with a submission ID using the client and logs the
#' responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Can be either an
#'   accession or provisional ID.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema with \code{get} methods. If \code{NULL},
#'   default client will be created by \code{create_client(extract_api())}.
#'   Defaults to \code{NULL}.
#' @param logfile A string specifying the path to a log file. If \code{NULL}, no
#'   log is written. Defaults to \code{NULL}.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses including submission data and associated
#'   datasets, analyses, runs, experiments, samples, and studies.
#'
#' @examples
#' mock_client <- list(
#'     "get__submissions__accession_id" = function(id) list(data = id),
#'     "get__submissions__accession_id__datasets" =
#'         function(id) list(datasets = id)
#' )
#' get_submission("EGAB12345678901", mock_client)
#'
#' @export
get_submission <- function(id, client = NULL, logfile = NULL, ...) {
    if (is_accession(id)) {
        base_url <- "submissions__accession_id"
    } else if (is_provisional(id)) {
        base_url <- "submissions__provisional_id"
    } else {
        stop("Unknown ID type, must be valid accession of provisional ID.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    responses <- c(
        # Include submission endpoint for GET method
        list(submission = client[[paste0("get", "__", base_url)]](id)),
        use_submission(id, "get", client)
    )

    save_log(responses, logfile)

    responses
}


#' Retrieve EGA entries by title
#'
#' Searches for entries across specified EGA metadata types that match a given
#' title string. Returns a list of data frames for each type.
#'
#' @param title Character scalar. The title or substring to search for.
#' @param type Character vector. One or more metadata types ("submissions",
#'   "studies", "samples", "experiments", "runs", "analyses" and "datasets"). If
#'   NULL, searches all valid types.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema. If \code{NULL}, default client will be
#'   created by \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param logfile Character. Path of log file to log the `httr2` responses from
#'   individual operations or \code{NULL}. Defaults to \code{NULL}.
#' @param ... List. Additional arguments to the function.
#'
#' @return A named list of data frames containing entries matching the title.
#'
#' @examples
#' # Requires credentials
#' try(
#'     get_entry_by_title("My Study", type = "studies")
#' )
#'
#' @export
get_entry_by_title <- function(
    title, type = NULL, client = NULL, logfile = NULL, ...
) {
    valid_types <- c(
        "submissions", "studies", "samples", "experiments", "runs", "analyses",
        "datasets"
    )

    if (is.null(type)) {
        type <- valid_types
    } else {
        if (!all(type %in% valid_types)) {
            stop(sprintf(
                "Invalid types specified, must be one of: %s.",
                paste(valid_types, collapse = ", ")
            ))
        }
    }

    .validate_character_scalar(title)

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    responses <- lapply(type, function(x) {
        sr <- client[[paste0("get__", x)]]()
        if ("title" %in% names(sr)) {
            sr <- sr[grepl(title, sr$title), ]
        } else {
            sr <- sr[FALSE, ]
        }
    }) |>
        setNames(type)

    save_log(responses, logfile)

    responses
}

#' Delete Submission Contents and Log Responses
#'
#' Deletes all data associated with a submission ID using the client and logs
#' the responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Can be either an
#'   accession or provisional ID.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema with \code{delete} methods. If \code{NULL},
#'   default client will be created by \code{create_client(extract_api())}.
#'   Defaults to \code{NULL}.
#' @param logfile A string specifying the path to a log file. If \code{NULL}, no
#'   log is written. Defaults to \code{NULL}.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses for the deletion of associated datasets,
#'   analyses, runs, experiments, samples, and studies.
#'
#' @examples
#' mock_client <- list(
#'     "delete__submissions__provisional_id__datasets" =
#'         function(id) list(status = "deleted")
#' )
#' delete_submission_contents(5678901, mock_client)
#'
#' @export
delete_submission_contents <- function(id, client = NULL, logfile = NULL, ...) {
    if (!is_provisional(id)) {
        stop("ID must be provisional ID.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    responses <- use_submission(id, client, "delete")
    save_log(responses, logfile)
    responses
}

#' Delete a Submission and Log Responses
#'
#' Deletes a submission identified by its ID using the client and logs the
#' response if a logfile is specified.
#'
#' @param id A string representing the submission identifier (provisional ID).
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema with \code{delete} method for submissions. If
#'   \code{NULL}, default client will be created by
#'   \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param client An API client object with a \code{delete} method for
#'   submissions.
#' @param logfile A string specifying the path to a log file. If \code{NULL}, no
#'   log is written. Defaults to \code{NULL}.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list containing the response for the submission deletion.
#'
#' @examples
#' mock_client <- list(
#'     delete__submissions__provisional_id =
#'         function(id) list(status = "deleted")
#' )
#' delete_submission("5678901", mock_client)
#'
#' @export
delete_submission <- function(id, client = NULL, logfile = NULL, ...) {
    if (!is_provisional(id)) {
        stop("ID must be provisional ID.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    responses <- list(
        submission = client$delete__submissions__provisional_id(id)
    )
    save_log(responses, logfile)
    responses
}

#' Rollback Submission Endpoints and Log Responses
#'
#' Rolls back specified endpoints for a submission identified by its accession
#' ID using the client and logs the responses if a logfile is specified.
#'
#' @param id A string representing the submission identifier. Must be an
#'   accession ID.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema with \code{put} methods and rollback
#'   operations. If \code{NULL}, default client will be created by
#'   \code{create_client(extract_api())}. Defaults to \code{NULL}.
#' @param endpoints A character vector of endpoint names to rollback.
#' @param logfile A string specifying the path to a log file. If \code{NULL}, no
#'   log is written. Defaults to \code{NULL}.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A list of responses from the rollback operations for each endpoint.
#'
#' @examples
#' mock_client <- list(
#'     "put__submissions__accession_id__datasets_rollback" =
#'         function(id) list(status = "rolled back")
#' )
#' rollback_submission("provisional123", mock_client, c("datasets"))
#'
#' @export
rollback_submission <- function(
    id, endpoints, client = NULL, logfile = NULL, ...
) {
    if (!is_accession(id)) {
        stop(
            "Incorrect format of accesssion ID.
            Following format is required: ^EGA\\d{11}$"
        )
    }

    if (!is.list(endpoints)) {
        stop("'endpoints' must be a list.")
    }

    lapply(endpoints, .validate_character_scalar, "All elements in endpoints")

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    responses <- lapply(endpoints, function(x) {
        endpoint_str <- paste0(
            "put__submissions__accession_id__",
            x,
            "__rollback"
        )
        client[[endpoint_str]](id)
    })

    save_log(responses, logfile)
    responses
}
