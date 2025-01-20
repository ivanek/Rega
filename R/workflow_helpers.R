#' Check if a String is a Valid Accession Identifier
#'
#' This function verifies whether the input string matches the format of a
#' valid accession identifier based on a specified schema.
#'
#' @param x A character vector to be tested for validity as an accession.
#' @param schema A character string specifying the schema. Valid options
#' include "study", "studies", "sample", "samples", "experiment",
#' "experiments", "analysis", "analyses", "run", "runs", "policy",
#' "DAC", "dataset", "datasets", and "submission". Defaults to "submission".
#'
#' @return A logical vector indicating whether each element of \code{x} is a
#' valid accession identifier for the specified schema.
#'
#' @examples
#' is_accession("EGA00000000001", "submission") # TRUE
#' is_accession("EGA12345678901", "sample") # FALSE
#'
#' @export
is_accession <- function(x, schema = "submission") {
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

  if (!schema %in% names(letter_lut)) {
    err_msg <- sprintf(
      "Unknown schema %s, please select one of the valid EGA schemas.", schema
    )
    stop(err_msg)
  }

  letter <- letter_lut[schema]
  grepl(paste0("^EGA", letter, "\\d{11}$"), x)
}


#' Generate a Step-by-Step Message Function
#'
#' Creates a closure function to display sequential progress messages for a
#' specified number of steps.
#'
#' @param steps An integer specifying the total number of steps.
#'
#' @return A function that takes a message string as input and displays it
#' along with the current step and total steps. The step count increments
#' automatically with each call.
#'
#' @examples
#' stepper <- step_msg(3)
#' stepper("Initializing") # "Step 1/3 - Initializing"
#' stepper("Processing") # "Step 2/3 - Processing"
#' stepper("Finalizing") # "Step 3/3 - Finalizing"
#'
#' @export
step_msg <- function(steps) {
  cur <- 1
  inner <- function(msg) {
    step_msg <- paste0("Step ", cur, "/", steps, " - ", msg)
    message(step_msg)
    assign("cur", cur + 1, envir = parent.env(environment()))
  }
  return(inner)
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
#' @examples
#' row <- data.frame(a = 1, b = "text", stringsAsFactors = FALSE)[1, ]
#' unbox_row(row)
#'
#' @importFrom jsonlite fromJSON toJSON unbox
#'
#' @export
unbox_row <- function(row) {
  unbox(fromJSON(toJSON(row)))
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
#' accept \code{id} and a JSON \code{body} as arguments.
#'
#' @return Data frame. A combined response object from the API.
#'
#' @examples
#' tab <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
#' mock_endpoint <- function(id, body) list(id = id, body = body)
#' submit_table(tab, "example_id", mock_endpoint)
#'
#' @export
submit_table <- function(tab, id, endpoint_func) {
  row_resp <- vector("list", length = nrow(tab))
  # For seems to work best/least problematic when maintaining structure to be
  # converted to JSON
  for (x in seq_len(nrow(tab))) {
    row_resp[[x]] <- endpoint_func(id, body = unbox_row(tab[x, ]))
  }
  row_resp <- do.call(rbind, row_resp)
  return(row_resp)
}

#' Retrieve or Submit Data to an EGA API Endpoint
#'
#' This function retrieves existing data from an API or submits new data if it
#' does not exist, with optional error handling and retrieval options.
#' - If no data is present in the database, it will supplied data will be
#' inserted.
#' - If there is data already present in the database and the number of records
#' don't match, error will raised.
#' - If the number of records match and \code{retrieve_if_exists} is set to TRUE
#' data will be retrieved from database and nothing will be inserted. If
#' \code{retrieve_if_exists} is set to FALSE, error will be raised.
#'
#' @param submission_id An integer representing the submission provisional ID.
#' @param data A data frame to be submitted.
#' @param client An API client object with \code{get} and \code{post} methods.
#' @param endpoint A string specifying the EGA API endpoint. The endpoint will
#' be a submission type endpoint identified with provisional ID.
#' @param retrieve_if_exists A logical flag indicating whether to retrieve data
#' if it already exists. Defaults to \code{FALSE}.
#'
#' @return A data frame containing the response from the API.
#'
#'
#' @export
get_or_post <- function(submission_id, data, client, endpoint,
                        retrieve_if_exists = FALSE) {
  built_url <- paste0("__", "submissions__provisional_id", "__", endpoint)
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
  } else if (retrieve_if_exists) {
    message("Retrieved IDs from database.")
  } else {
    err_msg <- sprintf(
      "%s records are already present in the database and 'retrive_if_exists'
      is set to FALSE", nrow(resp)
    )
    stop(err_msg)
  }
  return(resp)
}

#' Save API Responses to a Log File
#'
#' This function saves a list of API responses to a specified log file in YAML
#' format.
#'
#' @param responses A list of responses to be saved.
#' @param logfile A string specifying the path to the log file. If \code{NULL},
#' no file is written.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' responses <- list(status = "success", data = list(a = 1, b = "text"))
#' save_log(responses, "log.yaml")
#' }
#'
#' @importFrom yaml write_yaml
#'
#' @export
save_log <- function(responses, logfile) {
  if (!is.null(logfile)) {
    write_yaml(responses, logfile, column.major = FALSE)
  }
  return(invisible(NULL))
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
#' no file is written.
#' @param ... Additional expressions to evaluate when an error occurs.
#'
#' @return A function to handle errors during the specified workflow step.
#'
#' @examples
#' \dontrun{
#' handler <- workflow_error_handler("submission", list(), "log.yaml")
#' tryCatch(stop("Example error"), error = handler)
#' }
#'
#' @importFrom rlang enquos eval_tidy caller_env trace_back abort
#'
#' @export
workflow_error_handler <- function(step, responses, logfile, ...) {
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

  return(ef)
}
