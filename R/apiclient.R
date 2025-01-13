#' Extract API Specification and Host Details
#'
#' This function parses an API specification file (JSON or YAML) and extracts
#' relevant details.
#'
#' @param spec_file Character. Path to the API specification file in JSON or
#' YAML format.
#' @param host Character. Optional. The API host URL. If not supplied, it will
#' be inferred from the specification file's `servers` element.
#'
#' @return A list containing the parsed API specification, including the `host`
#' and `basePath` elements. If the specification file lacks required elements,
#' appropriate warnings or errors are raised.
#'
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # Extract API details from a YAML specification file
#' api <- extract_api("api_spec.yaml")
#'
#' # Extract API details with a custom host
#' api <- extract_api("api_spec.json", host = "https://api.example.com")
#' }
#'
#' @export
extract_api <- function(spec_file = NULL, host = NULL) {
  # if specification file is NULL, use the default one bundled with the package
  if (is.null(spec_file)) {
    spec_file <- system.file("extdata/ega_api_resolved.yaml", package = "Rega")
  }
  ext <- tolower(file_ext(spec_file))
  parse_fun <- switch(ext,
    yml = ,
    yaml = read_yaml,
    json = \(x) fromJSON(x, simplifyDataFrame = FALSE),
    stop("Specification file does not appear to be JSON or YAML.")
  )

  api <- parse_fun(spec_file)

  # used (including the port).
  if (is.null(host)) {
    server_urls <- vapply(api$servers, \(x) x$url, FUN.VALUE = character(1))
    if (length(server_urls) < 1) {
      stop("Host URL not supplied and not found in specification file.")
    }
    api$host <- server_urls[1]
  } else {
    api$host <- host
  }

  # If basepath is not included, the API is served directly under the host
  if (is.null(api$basePath)) {
    api$basePath <- ""
  }

  # remove the trailing "/" from base path
  api$basePath <- gsub("/$", "", api$basePath)

  if (is.null(api$paths)) {
    warning("There is no paths element in the API specification")
  }

  return(api)
}

#' Extract API Operation Definitions
#'
#' This function extracts operation definitions from an API specification,
#' including HTTP methods, paths, parameters, request bodies, and responses.
#'
#' @param api List. Parsed API specification, generated from a JSON or
#' YAML file. Must include a `paths` element with API endpoint definitions.
#'
#' @return A named list of operations, where each name corresponds to an
#' operation ID. If operation Id is not found in the specification, unique one
#' will be created. Each operation contains:
#' - `method`: HTTP method (e.g., GET, POST).
#' - `path`: Endpoint path.
#' - `parameters`: List of operation parameters.
#' - `requestBody`: Details of the request body (if any).
#' - `responses`: Possible responses for the operation.
#' - `security`: Security requirements for the operation.
#'
#' @examples
#' \dontrun{
#' # Extract operation definitions from a parsed API specification
#' operations <- extract_operation_definitions(api)
#' }
#'
#' @export
extract_operation_definitions <- function(api) {
  valid_methods <- c("post", "patch", "get", "head", "delete", "put")

  operations <- list()
  paths <- api$paths
  for (path in names(paths)) {
    methods <- paths[[path]]
    for (method in intersect(names(methods), valid_methods)) {
      operation <- methods[[method]]
      operationId <- operation$operationId
      if (is.null(operationId)) {
        # Generate a unique operationId if missing
        operationId <- paste0(tolower(method), "_", gsub("[/{}/]", "_", path))
        operationId <- gsub("^_", "", operationId)
        operationId <- gsub("_$", "", operationId)
      }
      operations[[operationId]] <- list(
        method = toupper(method),
        path = path,
        parameters = operation$parameters,
        requestBody = operation$requestBody,
        responses = operation$responses,
        security = operation$security
      )
    }
  }
  return(operations)
}

#' Convert Operation Parameters to Function Arguments
#'
#' This function transforms an operation's parameter definitions into a list of
#' function arguments. Required parameters are marked as missing arguments.
#'
#' @param op List. An operation definition containing a `parameters` element,
#' which is a list of parameter definitions. Each parameter should include a
#' `name` and an optional `required` field.
#'
#' @return A named list representing function arguments. Names correspond to
#' parameter names, with required parameters set to missing (`quote(expr = )`)
#' and others initialized to `NULL`.
#'
#' @examples
#' \dontrun{
#' # Convert operation parameters to function arguments
#' args <- Rega:::.operation_params_to_args(op)
#' }
#'
#' @keywords internal
.operation_params_to_args <- function(op) {
  parameters <- op$parameters
  # create a list of NULLs of the same length as parameters and initialize
  # the names
  args_list <- vector("list", length(parameters))
  names(args_list) <- vapply(parameters, \(x) x$name, FUN.VALUE = character(1))

  if (!is.null(parameters)) {
    for (param in parameters) {
      required <- param$required %||% FALSE

      # if parameter is required remove the value from the list of future formals
      if (required) {
        args_list[[param$name]] <- quote(expr = )
      }
    }
  }
  return(args_list)
}

#' Extract Operation Parameters by Location
#'
#' This function organizes API operation parameters into categories based on
#' their location (`path`, `query`, or `header`).
#'
#' @param op List. An operation definition containing a `parameters` element,
#' which is a list of parameter definitions. Each parameter should include
#' a `name` and an `in` field specifying its location.
#'
#' @return A named list with elements:
#' - `path`: Character vector of path parameter names.
#' - `query`: Character vector of query parameter names.
#' - `header`: Character vector of header parameter names.
#'
#' @examples
#' \dontrun{
#' # Extract parameters categorized by location
#' params <- Rega:::.get_operation_params(op)
#' }
#'
#' @keywords internal
.get_operation_params <- function(op) {
  # Setup the output list
  params <- list(
    path = character(),
    query = character(),
    header = character()
  )

  parameters <- op$parameters

  if (!is.null(parameters)) {
    for (p in parameters) {
      # Categorize parameters
      if (p$`in` %in% c("path", "query", "header")) {
        params[[p$`in`]] <- c(params[[p$`in`]], p$name)
      }
    }
  }
  return(params)
}

#' Generate URL Parameter Replacement Expressions
#'
#' This function creates a list of expressions to replace placeholders in a URL
#' with corresponding parameter values.
#'
#' @param path_params Character vector. Names of the parameters to replace in
#' the URL. Each name should correspond to a placeholder in the format `{param}`.
#' @param url Character. The URL containing placeholders to be replaced.
#'
#' @return A list of expressions. Each expression replaces a `{param}`
#' placeholder in the URL with the value of the corresponding parameter.
#'
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Generate replacement expressions for path parameters
#' replacers <- Rega:::.url_param_replacer(c("id", "type"), "https://api/{id}/{type}")
#' }
#'
#' @keywords internal
.url_param_replacer <- function(path_params, url) {
  replacers <- lapply(path_params, function(param_name) {
    replace_expr <- bquote(
      url <- sub(
        .(paste0("{", param_name, "}")),
        as.character(.(sym(param_name))),
        url,
        fixed = TRUE
      )
    )
  })
  return(replacers)
}

#' Generate Header Expressions for an API Request
#'
#' This function creates expressions to add headers to an API request, including
#' content type, authorization, and any additional headers specified in the
#' parameters.
#'
#' @param params List. A list containing `header_params`, a character vector of
#' header parameter names to include in the request.
#' @param operation List. The operation definition, which may include security
#' details.
#' @param api List. The API definition, which may include global security
#' details and other metadata.
#'
#' @return An expression to add headers to an API request using `req_headers()`.
#'
#' @importFrom rlang syms expr
#' @importFrom httr2 req_url_query req_headers
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Generate header expressions for a request
#' headers_expr <- Rega:::.add_headers(params, operation, api)
#' }
#'
#' @keywords internal
.add_headers <- function(params, operation, api, token = NULL) {
  api_key <- NULL # for linting
  # Add headers
  headers_list <- list(
    `Content-Type` = "application/json"
  )
  if ((!is.null(operation$security) || !is.null(api$security)) &&
    !is.null(token)) {
    # Assuming API key authentication in header
    headers_list[["Authorization"]] <- expr(paste("Bearer", api_key))
  }
  if (length(params$header_params) > 0) {
    header_syms <- setNames(syms(params$header_params), params$header_params)
    headers_list <- c(headers_list, header_syms)
  }
  headers_expr <- expr(req <- req_headers(req, !!!headers_list))
}

#' Generate Query Parameter Expressions for an API Request
#'
#' This function creates an expression to add query parameters to an API
#' request.
#'
#' @param params List. A list containing `query`, a character vector of query
#' parameter names to include in the request.
#'
#' @return An expression to add query parameters to an API request using
#' `req_url_query()`.
#'
#' @importFrom rlang syms expr
#' @importFrom httr2 req_url_query
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Generate query parameter expressions for a request
#' query_expr <- Rega:::.add_queries(params)
#' }
#'
#' @keywords internal
.add_queries <- function(params) {
  # query_syms <- set_names(syms(params$query), params$query)
  query_syms <- setNames(syms(params$query), params$query)
  query_expr <- expr(
    req <- req_url_query(req, !!!query_syms)
  )
  return(query_expr)
}


#' Generate an API Function from Operation and Specification
#'
#' This function dynamically creates an API function based on a given operation
#' definition and API specification. The generated function handles URL
#' construction, parameter validation, request execution, and response parsing.
#'
#' @param operation List. The API operation definition, including method, path,
#' parameters, and request body schema.
#' @param api List. The API specification, including host and global security
#' definitions.
#' @param verbosity Integer, optional, values 0-3. Indicates with which verbosity
#' level should the requests \code{httr2::req_perform} be performed. Default: 0.
#' @param api_key Character, optional. The API key for authentication, included
#' in the headers of the request.
#'
#' @return A dynamically generated function that performs the specified API
#' operation. The function accepts arguments corresponding to operation
#' parameters and executes the request using `httr2`.
#'
#' @importFrom rlang pairlist2 expr new_function caller_env !! !!!
#' @importFrom httr2 req_method request req_body_json req_perform resp_check_status
#' @examples
#' \dontrun{
#' # Generate an API function for a specific operation
#' api_func <- api_function_factory(operation, api, api_key = "your_api_key")
#'
#' # Call the generated function with parameters
#' result <- api_func(param1 = "value1", param2 = "value2")
#' }
#'
#' @export
api_function_factory <- function(operation, api, verbosity = 0, api_key = NULL) {
  resp <- NULL # lint
  base_url <- api$host
  op <- operation

  # Prepare function arguments
  has_body <- !is.null(op$requestBody)

  # Build the function arguments based on api operation -----
  func_args <- list()
  func_args <- c(func_args, .operation_params_to_args(op))
  if (has_body) func_args <- c(func_args, pairlist2(body = ))
  # Add api key as the last function argument
  if (!is.null(api_key)) func_args <- c(func_args, pairlist2(api_key = api_key))

  # Build function body -----
  body_exprs <- list()

  if (has_body) {
    schema <- get_operation_schema(op)
    if (!is.null(schema)) {
      validate_expr <- expr(
        valid <- validate_schema(body, !!schema)
      )
      stop_expr <- expr(if (!valid) {
        stop(validation_to_msg(valid), call. = FALSE)
      })
      body_exprs <- c(body_exprs, validate_expr, stop_expr)
    }
  }

  # Create full URL and add it to the function body
  url <- paste0(base_url, op$path)
  body_exprs <- c(body_exprs, expr(url <- !!url))

  params <- .get_operation_params(op)
  # If there are parameters in the path, add the URL substitution into the
  # function body
  if (length(params$path) > 0) {
    body_exprs <- c(body_exprs, .url_param_replacer(params$path))
  }

  # Build the request -----
  req_expr <- bquote(req <- req_method(request(url), .(op$method)))
  body_exprs <- c(body_exprs, req_expr)

  if (is.null(api_key)) {
    body_exprs <- c(body_exprs, expr(req <- ega_oauth(req)))
  }

  # Add headers
  body_exprs <- c(body_exprs, .add_headers(params, op, api, api_key))

  # If there were query parameters added to the function formals, add appropriate
  # code to the body in the form of httr2 query
  if (length(params$query) > 0) {
    body_exprs <- c(body_exprs, .add_queries(params))
  }

  # Add request body -----
  if (has_body) {
    body_request_expr <- expr(req <- req_body_json(req, body, auto_unbox = FALSE))
    body_exprs <- c(body_exprs, body_request_expr)
  }

  # Perform the request and handle the response -----
  perform_req <- list(
    bquote(resp <- req_perform(req, verbosity = .(verbosity))),
    expr(resp_check_status(resp)),
    expr(result <- parse_ega_body(resp)),
    expr(return(result))
  )

  body_exprs <- c(body_exprs, perform_req)

  # Combine body expressions into one expression
  func_body <- expr({
    !!!body_exprs
  })

  # Create the function based on formals, body and env
  func <- new_function(
    args = as.pairlist(func_args),
    body = func_body,
    env = caller_env()
  )

  return(func)
}

#' Generate API Client Functions
#'
#' This function creates a named list of functions for interacting with an API,
#' based on its specification and operation definitions.
#'
#' @param api List. The API specification, including operation definitions,
#' host, and global settings.
#' @param ... List. List of additional arguments passed to
#' \code{api_function_factory}
#'
#' @return A named list of functions, where each function corresponds to an API
#' operation. The function names match the operation IDs from the specification.
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Create an API client
#' client <- create_client(api, api_key = "your_api_key")
#'
#' # Call an operation using the client
#' result <- client$operation_id(param1 = "value1", param2 = "value2")
#' }
#'
#' @export
create_client <- function(api, ...) {
  opdefs <- extract_operation_definitions(api)
  setNames(
    lapply(opdefs, \(x) api_function_factory(x, api, ...)),
    names(opdefs)
  )
}

#' Parse The Information From EGA httr2 Response Object.
#'
#' Parses the body of a body of `httr2` response object from the EGA API,
#' handling JSON and plain text content, and formats it into a tibble for
#' further processing.
#'
#' @param resp An HTTP response object from the EGA API.
#'
#' @return A tibble containing the parsed and formatted response data. If the
#' response is plain text without a JSON-like structure, a one-column tibble
#' is returned with the raw content.
#'
#' @importFrom httr2 resp_url_path resp_body_json resp_body_string resp_content_type
#' @importFrom jsonlite fromJSON
#' @importFrom rlang :=
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#'
#' @examples \dontrun{
#' url <- "https://submission.ega-archive.org/api/files"
#' req <- req_method(request(url), "GET")
#' req <- Rega:::ega_oauth(req)
#' req <- req_headers(req, `Content-Type` = "application/json")
#' req <- req_url_query(req, status = NULL, prefix = NULL)
#' resp <- req_perform(req, verbosity = 3)
#' Rega:::ega_parse_body(resp)
#' }
#'
#' @keywords internal
parse_ega_body <- function(resp) {
  resource_name <- resp |>
    resp_url_path() |>
    str_replace("\\/api\\/(\\w+)\\/?.*", "\\1")

  if (resp_content_type(resp) == "application/json") {
    # check if the content is JSON
    resp <- resp_body_json(resp)

    # special treatment for short list (e.g. user info)
    if (!is.null(names(resp))) resp <- list(resp)
  } else if (resp_content_type(resp) == "text/plain") {
    # if plain text, convert to JSON list
    resp <- resp_body_string(resp)

    if (grepl("^\\{.*\\}$", resp)) {
      # if there is JSON like structure
      resp <- resp |>
        fromJSON() |>
        lapply(function(x) if (is.null(x)) list() else x) |>
        list()
    } else {
      # in case there is no JSON like structure
      # return the value as an one-column tibble
      return(tibble("{resource_name}" := resp))
    }
  }

  resp <- resp |>
    tibble() |>
    unnest_wider(resp, names_sep = "/", names_repair = "unique")
  # in the datasets response, there are 2 columns with status, why?

  if (ncol(resp) == 1) {
    names(resp)[names(resp) == "resp"] <- resource_name
  }

  # remove anything before slash .*/from column names
  colnames(resp) <- sub(".*\\/", "", colnames(resp))

  return(resp)
}
