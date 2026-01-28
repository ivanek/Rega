#' Check if a String is a Valid Accession Identifier.
#'
#' Verifies whether the input string matches the format of a valid accession
#' identifier based on a specified schema.
#'
#' @param x A character vector to be tested for validity as accessions.
#' @param schema A character string specifying the schema. Valid options include
#'   "study", "studies", "sample", "samples", "experiment", "experiments",
#'   "analysis", "analyses", "run", "runs", "policy", "DAC", "dataset",
#'   "datasets", "submission" and \code{NULL}. \code{NULL} will check against
#'   any of the schemas. Defaults to \code{NULL}.
#'
#' @return A logical vector indicating which values are accession IDs.
#'
#' @importFrom rlang is_empty
#'
#' @examples
#' is_accession("EGAB00000000001", "submission") # TRUE
#' is_accession("EGA12345678901", "sample") # FALSE
#'
#' @export
is_accession <- function(x, schema = NULL) {
    letter_lut <- c(
        "study" = "S",
        "studies" = "S",
        "sample" = "N",
        "samples" = "N",
        "experiment" = "X",
        "experiments" = "X",
        "analysis" = "Z",
        "analyses" = "Z",
        "run" = "R",
        "runs" = "R",
        "policy" = "P",
        "DAC" = "C",
        "dataset" = "D",
        "datasets" = "D",
        "submission" = "B"
    )

    if (is_empty(x)) {
        return(FALSE)
    }

    if (is.null(schema)) {
        letter <- paste0("[", paste(unique(letter_lut), collapse = "|"), "]")
    } else if (schema %in% names(letter_lut)) {
        letter <- letter_lut[schema]
    } else {
        err_msg <- sprintf(
            "Unknown schema %s, please select one of the valid EGA schemas.",
            schema
        )
        stop(err_msg)
    }

    grepl(paste0("^EGA", letter, "\\d{11}$"), x)
}

#' Check whether IDs is a provisional ID.
#'
#' Determine if input values match the format of provisional IDs, either as
#' whole-number numerics or as character strings of at least two digits without
#' leading zeros.
#'
#' @param x A numeric or character vector of candidate provisional IDs.
#'
#' @return A logical vector indicating which values are provisional IDs.
#'
#' @importFrom rlang is_empty
#'
#' @examples
#' is_provisional(c(10, 11, 3.5, 9))
#'
#' @export
is_provisional <- function(x) {
    if (is_empty(x)) {
        return(FALSE)
    }

    if (is.numeric(x)) {
        as.numeric(x) == as.integer(x)
    } else if (is.character(x)) {
        grepl("^[1-9][0-9]*$", x)
    } else {
        stop(
            "Unknown type of provisional ID.
            Can be either numeric or character."
        )
    }
}

#' Generate a Step-by-Step Message Function
#'
#' Creates a closure function to display sequential progress messages for a
#' specified number of steps.
#'
#' @param steps An integer specifying the total number of steps.
#'
#' @return A function that takes a message string as input and displays it along
#'   with the current step and total steps. The step count increments
#'   automatically with each call.
#'
#' @examples
#' stepper <- step_msg(3)
#' stepper("Initializing") # "Step 1/3 - Initializing"
#' stepper("Processing") # "Step 2/3 - Processing"
#' stepper("Finalizing") # "Step 3/3 - Finalizing"
#'
#' @export
step_msg <- function(steps) {
    if (steps <= 0) stop("'steps' must be a positive integer.")
    if (!is.numeric(steps) || length(steps) != 1) {
        stop("'steps' must be numeric of length 1.")
    }

    cur <- 1
    inner <- function(msg) {
        step_msg <- paste0("Step ", cur, "/", steps, " - ", msg)
        message(step_msg)
        assign("cur", cur + 1, envir = parent.env(environment()))
    }
    inner
}

#' Convert a Data Frame Row to an Unboxed JSON Object
#'
#' This function converts a single row of a data frame into an unboxed JSON
#' object, effectively removing the array structure.
#'
#' @param row A single row of a data frame.
#'
#' @return A JSON object with unboxed values for the input row.
#'
#' @importFrom jsonlite fromJSON toJSON unbox
#'
#' @examples
#' row <- data.frame(a = 1, b = "text", stringsAsFactors = FALSE)[1, ]
#' unbox_row(row)
#'
#' @export
unbox_row <- function(row) {
    if (!is.vector(row) && !is.list(row)) {
        stop(
            "The 'row' argument must be a vector or a list
            (e.g., a data frame row)."
        )
    }
    unbox(fromJSON(toJSON(row)))
}

#' Convert a List to an Unboxed JSON-Compatible Data Frame
#'
#' Converts a list into a single-row data frame with unboxed elements if all
#' elements have a length of 1. Otherwise, an error is raised.
#'
#' @param l A list where all elements must have a length of 1.
#'
#' @return A data frame with unboxed elements, suitable for JSON conversion.
#'
#' @examples
#' input_list <- list(a = 1, b = "text", c = TRUE)
#' unbox_list(input_list)
#'
#' @export
unbox_list <- function(l) {
    if (!is.list(l)) {
        stop("'l' must be a list.")
    }

    is_l1 <- all(vapply(l, \(x) length(x) == 1, logical(1)))
    if (is_l1) {
        row <- unbox_row(as.data.frame(l))
    } else {
        stop("All elements of the list must be of length 1.")
    }
    row
}

#' Submit a Data Frame to an API Endpoint Row by Row
#'
#' This function iterates over rows of a data frame, submitting each row to a
#' specified API endpoint function, and combines the responses into a single
#' data structure.
#'
#' @param tab A data frame containing the data to be submitted.
#' @param id An EGA accession/provisional ID passed to the \code{endpoint_func}.
#' @param endpoint_func A function that handles the API request. It should
#'   accept \code{id} and a JSON \code{body} as arguments.
#'
#' @return Data frame. A combined response object from the API.
#'
#' @examples
#' tab <- data.frame(a = 1:2, b = c("x", "y"))
#' mock_endpoint <- function(id, body) list(id = id, body = body)
#' submit_table(tab, 12345, mock_endpoint)
#'
#' @export
submit_table <- function(tab, id, endpoint_func) {
    if (!is.data.frame(tab)) stop("'tab' must be a data frame.")

    if (nrow(tab) == 0) {
        stop("'tab' has zero rows.")
    }

    if (!is_provisional(id) && !is_accession(id)) {
        stop("'submission_id' must be either provisional or accession ID.")
    }

    if (!is.function(endpoint_func)) {
        stop("The 'endpoint_func' must be a function.")
    }

    row_resp <- vector("list", length = nrow(tab))
    # For loop seems to work best/least problematic when maintaining structure
    # to be converted to JSON
    for (x in seq_len(nrow(tab))) {
        row_resp[[x]] <- endpoint_func(id, body = unbox_row(tab[x, ]))
    }

    row_resp <- do.call(rbind, row_resp)
    row_resp
}

#' Retrieve or Submit Data to an EGA API Endpoint
#'
#' This function retrieves existing data from an API or submits new data if it
#' does not exist, with optional error handling and retrieval options.
#' - If no data is present in the database, supplied data will be
#' inserted.
#' - If there is data already present in the database and the number of records
#' don't match, error will raised.
#' - If the number of records match and \code{retrieve} is set to TRUE
#' data will be retrieved from database and nothing will be inserted. If
#' \code{retrieve} is set to FALSE, error will be raised.
#'
#' @param submission_id An integer representing the submission provisional ID.
#' @param data A data frame to be submitted.
#' @param client An API client object with \code{get} and \code{post} methods.
#' @param endpoint A string specifying the EGA API endpoint. The endpoint will
#'   be a submission type endpoint identified with provisional ID.
#' @param retrieve A logical flag indicating whether to retrieve data
#'   if it already exists. Defaults to \code{FALSE}.
#' @param id_type A string specifying type of EGA id. One of 'provisional' or
#'   'accession'. Defaults to \code{provisional}.
#'
#' @return A data frame containing the response from the API.
#'
#' @examples
#' # Create mock client for API endpoint
#' mock_client <- list(
#'     get__submissions__provisional_id__endpoint = function(id) {
#'         message("Mock GET request")
#'         # Simulate an empty response (no existing data)
#'         return(NULL)
#'     },
#'     post__submissions__provisional_id__endpoint = function(id, body) {
#'         message("Mock POST request")
#'         message(body) # Simulate returning submitted data
#'     }
#' )
#'
#' # Create mock data to test the function
#' test_data <- data.frame(id = 1:3, value = c("A", "B", "C"))
#'
#' # Test the function with mock data and client
#' result <- get_or_post(
#'     submission_id = 12345,
#'     data = test_data,
#'     client = mock_client,
#'     endpoint = "endpoint",
#'     retrieve = FALSE
#' )
#'
#' @export
get_or_post <- function(
    submission_id, data, client, endpoint, retrieve = FALSE,
    id_type = "provisional"
) {
    if (!is_provisional(submission_id) && !is_accession(submission_id)) {
        stop("'submission_id' must be either provisional or accession ID.")
    }
    if (!is.data.frame(data)) stop("'data' must be a data frame.")
    .is_client(client)
    .validate_character_scalar(endpoint)
    .validate_character_scalar(id_type)
    .validate_logical_scalar(retrieve, "retrieve")

    built_url <- paste0("__", "submissions__", id_type, "_id", "__", endpoint)
    resp <- client[[paste0("get", built_url)]](submission_id)

    if (is.null(resp) || nrow(resp) == 0) {
        # Submit table
        resp <- submit_table(
            data,
            submission_id,
            client[[paste0("post", built_url)]]
        )
    } else if (nrow(resp) != nrow(data)) {
        err_msg <- sprintf(
            "Number of present versus submitted records doesn't match: %s, %s",
            nrow(resp), nrow(data)
        )
        stop(err_msg)
    } else if (retrieve) {
        message("Retrieved IDs from database.")
    } else {
        err_msg <- sprintf(
            paste(
                "%s records are already present in the database and",
                "'retrive_if_exists' is set to FALSE"
            ), nrow(resp)
        )
        stop(err_msg)
    }
    resp
}

#' Save API Responses to a Log File
#'
#' This function saves a list of API responses to a specified log file in YAML
#' format.
#'
#' @param responses A list of responses to be saved.
#' @param logfile A string specifying the path to the log file. If \code{NULL},
#'   no file is written.
#'
#' @return Invisibly returns \code{NULL}
#'
#' @importFrom yaml write_yaml
#'
#' @examples
#' responses <- list(status = "success", data = list(a = 1, b = "text"))
#' save_log(responses, logfile = NULL)
#'
#' @export
save_log <- function(responses, logfile) {
    if (!is.list(responses) && !is.vector(responses)) {
        stop(
            "The 'responses' argument must be a list (or vector coercible
                to a list)."
        )
    }

    if (!is.null(logfile)) {
        .validate_character_scalar(logfile)
        if (dir.exists(logfile)) stop("Specified 'logfile' is a direcory.")
        write_yaml(responses, logfile, column.major = FALSE)
    }

    invisible(NULL)
}

#' Workflow Error Handler
#'
#' Creates a custom error handler for managing errors during a workflow step.
#' Logs responses, executes additional expressions, and stops execution with a
#' detailed message and a stack trace.
#'
#' @param step A string representing the current workflow step.
#' @param responses A list of responses to be logged in case of an error.
#' @param logfile A string specifying the path to the log file. If \code{NULL},
#'   no file is written.
#' @param ... Additional expressions to evaluate when an error occurs.
#'
#' @return A function to handle errors during the specified workflow step.
#'
#' @importFrom rlang enquos eval_tidy caller_env trace_back abort
#'
#' @examples
#' handler <- workflow_error_handler(
#'     step = "submission",
#'     responses = list(),
#'     logfile = NULL
#' )
#'
#' tryCatch("Example code without error", error = handler)
#'
#' @export
workflow_error_handler <- function(step, responses, logfile, ...) {
    # Responses and logfile checks are in save_log function
    .validate_character_scalar(step)

    captured_exprs <- enquos(...)

    em <- paste0("Error while creating ", step, ".")

    if (step != "submission") {
        em <- paste0(em, " All ", step, " were removed.")
    }

    ef <- function(e) {
        message(em)
        save_log(responses, logfile)
        lapply(captured_exprs, \(x) eval_tidy(x, env = caller_env()))
        abort(e$message, trace = trace_back())
    }

    ef
}

#' Execute a submission step with error handling and rollback
#'
#' Wraps a logic function in a tryCatch block to handle errors during a specific
#' submission step, optionally triggering a rollback function.
#'
#' @param step_name Character. The name of the current workflow step.
#' @param logic_fn Function. The primary logic to execute for this step.
#' @param rollback_fn Function. A function to clean up if an error occurs.
#' @param responses List. Current collection of API responses for logging.
#' @param logfile Character. Path to the log file.
#'
#' @return The result of `logic_fn()`.
#'
#' @examples
#' try_step(
#'     "test", function() 1 + 1, function() print("fail"), list(), "log.txt"
#' )
#'
#' @export
try_step <- function(step_name, logic_fn, rollback_fn, responses, logfile) {
    .validate_character_scalar(step_name)

    if (!is.function(logic_fn)) {
        stop("'logic_fn' must be a function.")
    }

    if (!is.list(responses)) {
        stop("'responses' must be a list.")
    }

    if (!is.null(logfile) && (!is.character(logfile) || length(logfile) != 1)) {
        stop("'logfile' must be a non-empty character scalar or NULL.")
    }

    tryCatch(
        {
            withCallingHandlers(
                logic_fn(),
                error = workflow_error_handler(
                    step_name, responses, logfile, rollback_fn
                )
            )
        },
        error = function(e) stop(e)
    )
}

#' Fetch file information from EGA inbox
#'
#' Query the remote client for requested file prefix, test whether a file is
#' found for every element of \code{file_list} and return the server response.
#'
#' @param file_list A character vector or list of file prefixes to check.
#' @param client List of functions. EGA API client created by `create_client`
#'   function from EGA API schema. If \code{NULL}, default client will be
#'   created by \code{create_client(extract_api())}. Defaults to \code{NULL}.
#'
#' @return Data frame. Parsed response from client API for requested files.
#'
#' @examples
#' mock_client <- list(
#'     get__files = function(prefix = NULL) list(prefix)
#' )
#' fetch_files(c("file_a", "file_b"), mock_client)
#'
#' @export
fetch_files <- function(file_list, client = NULL) {
    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    if (!length(file_list)) {
        stop("'file_list' must contain at least one element.")
    }

    # Retrieve metadata
    inbox_files <- do.call(
        rbind,
        lapply(file_list, \(x) client$get__files(prefix = x))
    )

    # Verification
    if (nrow(inbox_files) != length(file_list)) {
        stop(sprintf(
            "Following files are missing from inbox: %s",
            paste(
                setdiff(file_list, inbox_files$relative_path),
                collapse = ", "
            )
        ))
    }

    list(
        response = inbox_files,
        lut = setNames(inbox_files$provisional_id, file_list)
    )
}

#' Check if sample aliases exist in the EGA database
#'
#' Validates uniqueness of sample aliases by comparing input against existing
#' records in the EGA database. Throws an error if duplicates are found and
#' retrieval is not enabled.
#'
#' @param samples Character vector of sample aliases to check.
#' @param client An EGA API client object. If NULL, one is created.
#' @param retrieve Logical scalar. If TRUE, exists without error even if samples
#'   are found in the database.
#'
#' @return Logical TRUE if validation passes.
#'
#' @examples
#' samples_in_db(c("sample1", "sample2"), client = my_client, retrieve = FALSE)
#'
#' @export
samples_in_db <- function(samples, client = NULL, retrieve = FALSE) {
    if (!length(samples) || !is.character(samples)) {
        stop("'samples' must be a character vector with at least one element.")
    }

    if (is.null(client)) {
        client <- create_client(extract_api())
    } else {
        .is_client(client)
    }

    .validate_logical_scalar(retrieve)

    user_samples <- client$get__samples()
    db_samples <- samples %in% user_samples$alias

    if (any(db_samples) && !retrieve) {
        err_msg <- sprintf(
            paste(
                "Samples aliases per submitter must be unique. Following",
                "sample aliases were found in EGA database: %s."
            ),
            paste(
                samples[db_samples],
                collapse = ", "
            )
        )
        stop(err_msg)
    }

    TRUE
}
