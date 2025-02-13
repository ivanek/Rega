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
#' # Extract API details from a default YAML specification file
#' api <- extract_api()
#'
#' # Extract API details with a custom host
#' api <- extract_api(host = "https://api.example.com")
#'
#' @export
extract_api <- function(spec_file = NULL, host = NULL) {
    # if specification file is NULL, use the default bundled with the package
    if (is.null(spec_file)) {
        spec_file <- system.file(
            "extdata/ega_api_resolved.yaml",
            package = "Rega"
        )
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
#' # Extract operation definitions from a parsed API specification
#' opdefs <- extract_operation_definitions(extract_api())
#' opdefs[["post__submissions"]]
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
            operation_id <- operation$operation_id
            if (is.null(operation_id)) {
                # Generate a unique operation_id if missing
                operation_id <- paste0(
                    tolower(method),
                    "_",
                    gsub("[/{}/]", "_", path)
                )
                operation_id <- gsub("^_", "", operation_id)
                operation_id <- gsub("_$", "", operation_id)
            }
            operations[[operation_id]] <- list(
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

#' Validate HTTP Method
#'
#' Checks whether a given HTTP method is valid based on a predefined list of
#' accepted methods (matches on lowercase).
#'
#' @param m A string representing the HTTP method to validate.
#'
#' @return A logical value: \code{TRUE} if \code{m} is a valid HTTP method,
#'   otherwise \code{FALSE}.
#'
#' @examples
#' is_valid_http_method("GET")    # TRUE
#' is_valid_http_method("get")    # TRUE
#' is_valid_http_method("DELETE") # TRUE
#' is_valid_http_method("foo")    # FALSE
#' is_valid_http_method(NULL)     # FALSE
#' @export
is_valid_http_method <- function(m) {
    valid_methods <- c("post", "patch", "get", "head", "delete", "put")

    if (is.null(m)) {
        return(FALSE)
    }

    if (!tolower(m) %in% valid_methods) {
        return(FALSE)
    }

    return(TRUE)
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
#' # Convert operation parameters to function arguments
#' opdefs <- extract_operation_definitions(extract_api())
#'
#' Rega:::.operation_params_to_args(
#'     opdefs[["post__submissions__provisional_id__samples"]]
#' )
#'
#' @keywords internal
.operation_params_to_args <- function(op) {
    parameters <- op$parameters
    # create a list of NULLs of the same length as parameters and initialize
    # the names
    args_list <- vector("list", length(parameters))
    names(args_list) <- vapply(
        parameters,
        \(x) x$name,
        FUN.VALUE = character(1)
    )

    if (!is.null(parameters)) {
        for (param in parameters) {
            required <- param$required %||% FALSE

            # if parameter is required remove the value from the list of formals
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
#' # Convert operation parameters to function arguments
#' opdefs <- extract_operation_definitions(extract_api())
#'
#' # Extract parameters categorized by location
#' Rega:::.get_operation_params(opdefs[["get__files"]])
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
            if (is.null(p$name)) {
                stop("Parameter needs a 'name' value.")
            }

            # Categorize parameters
            if (p$`in` %in% c("path", "query", "header")) {
                params[[p$`in`]] <- c(params[[p$`in`]], p$name)
            }
        }
    }
    return(params)
}

#' Generate URL Parameter Replacement Expressions for an API Request
#'
#' This function creates a list of expressions to replace placeholders in a URL
#' with corresponding parameter values.
#'
#' @param path_params A character vector of names of the parameters to replace
#'   in the URL. Each name should correspond to a placeholder in the URL in the
#'   format `{param}`. Path parameters are created `.get_operation_params`
#'   function.
#'
#' @return A list of expressions. Each expression replaces a `{param}`
#'   placeholder in the URL with the value of the corresponding parameter.
#'
#' @importFrom rlang sym
#'
#' @examples
#' # Generate replacement expressions for path parameters
#' Rega:::.add_paths(c("id", "type"))
#'
#' @keywords internal
.add_paths <- function(path_params) {
    if (!all(vapply(path_params, is.character, logical(1)))) {
        stop("All 'path_params' must be character")
    }

    if (length(path_params) > 0) {
        rep_urls <- lapply(path_params, function(param_name) {
            bquote(
                url <- sub(
                    .(paste0("{", param_name, "}")),
                    as.character(.(sym(param_name))),
                    url,
                    fixed = TRUE
                )
            )
        })
        return(rep_urls)
    } else {
        return(list())
    }
}

#' Generate Header Expressions for an API Request
#'
#' This function creates expressions to add headers to an API request, including
#' content type, authorization, and any additional headers specified in the
#' parameters.
#'
#' @param header_params A character vector of header parameter names to include
#'   in the request. Header parameters are created `.get_operation_params`
#'   function.
#' @param operation List. The operation definition, which may include security
#'   details.
#' @param api List. The API definition, which may include global security
#'   details and other metadata.
#'
#' @return An expression to add headers to an API request using `req_headers()`.
#'
#' @importFrom rlang syms expr
#' @importFrom httr2 req_url_query req_headers
#' @importFrom stats setNames
#'
#' @examples
#' api <- extract_api()
#' opdefs <- extract_operation_definitions(api)
#' params <- Rega:::.get_operation_params(opdefs[["get__files"]])
#' # No header parameters in operation, `Content-Type` added by default
#' Rega:::.add_headers(params$header, opdefs[["get__files"]], api)
#'
#' @keywords internal
.add_headers <- function(header_params, operation, api, token = NULL) {
    # token variable is only used to check whether api key is being passed into
    # the function
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
    if (length(header_params) > 0) {
        header_syms <- setNames(syms(header_params), header_params)
        headers_list <- c(headers_list, header_syms)
    }

    return(expr(req <- req_headers(req, !!!headers_list)))
}

#' Generate Query Expressions for an API Request
#'
#' This function creates an expression to add query parameters to an API
#' request.
#'
#' @param query_params A character vector of query parameter names to include
#'   in the request. Query parameters are created `.get_operation_params`
#'   function.
#'
#' @return An expression to add query parameters to an API request using
#'   `req_url_query()`.
#'
#' @importFrom rlang syms expr
#' @importFrom httr2 req_url_query
#' @importFrom stats setNames
#'
#' @examples
#' Rega:::.add_queries(list())
#'
#' opdefs <- extract_operation_definitions(extract_api())
#' params <- Rega:::.get_operation_params(opdefs[["get__files"]])
#' Rega:::.add_queries(params$query)
#'
#' @keywords internal
.add_queries <- function(query_params) {
    if (length(query_params) > 0) {
        query_syms <- setNames(syms(query_params), query_params)
        query_expr <- expr(
            req <- req_url_query(req, !!!query_syms)
        )
        return(query_expr)
    } else {
        return(list())
    }
}

#' Add JSON Schema Validation to API Operation
#'
#' Generates validation expressions for an API operation based on its JSON
#' schema. If a schema is present, the function returns expressions to validate
#' the request body and raise an error if validation fails.
#'
#' @param op A list representing the API operation, which may contain a
#'   request body and schema.
#'
#' @return A list of expressions for JSON schema validation, or an empty list
#'   if no schema is found.
#'
#' @importFrom rlang expr
#'
#' @examples
#' \dontrun{
#' op <- list(requestBody = TRUE, schema = list(type = "object"))
#' Rega:::.add_json_validation(op)
#' }
#'
#' @keywords internal
.add_json_validation <- function(op) {
    has_body <- !is.null(op$requestBody)
    schema <- get_operation_schema(op)

    if (has_body && !is.null(schema)) {
        validate_expr <- expr(
            valid <- validate_schema(body, !!schema)
        )
        stop_expr <- expr(if (!valid) {
            stop(validation_to_msg(valid), call. = FALSE)
        })
        return(list(validate_expr, stop_expr))
    } else {
        return(list())
    }
}

#' Add Request Body to API Request
#'
#' Adds a JSON request body to an API request if required. If the request
#' requires a body, an expression is returned to include it; otherwise, an
#' empty list is returned.
#'
#' @param has_body A logical value indicating whether the request requires a
#'   body.
#'
#' @return An expression to add the JSON request body if \code{has_body} is
#'   \code{TRUE}, otherwise an empty list.
#'
#' @importFrom rlang expr
#'
#' @examples
#' Rega:::.add_request_body(TRUE)
#' Rega:::.add_request_body(FALSE)
#'
#' @keywords internal
.add_request_body <- function(has_body) {
    if (!is.logical(has_body)) stop("'has_body' must be logical.")

    if (has_body) {
        return(expr(req <- req_body_json(req, body, auto_unbox = FALSE)))
    } else {
        return(list())
    }
}

#' Generate an API Function from Operation and Specification
#'
#' This function dynamically creates an API function based on a given operation
#' definition and API specification. The generated function handles URL
#' construction, parameter validation, request execution, and response parsing.
#'
#' @param op List. The API operation definition, including method, path,
#'   parameters, and request body schema.
#' @param api List. The API specification, including host and global security
#'   definitions.
#' @param verbosity Integer, optional, values 0-3. Indicates with which
#'   verbosity level should the requests \code{httr2::req_perform} be performed.
#'   Default: 0.
#' @param api_key Character, optional. The API key for authentication, will be
#'   included in the headers of the request.
#'
#' @return A dynamically generated function that performs the specified API
#'   operation. The function accepts arguments corresponding to operation
#'   parameters and executes the request using `httr2`.
#'
#' @importFrom rlang pairlist2 expr new_function caller_env !! !!!
#' @importFrom httr2 req_method request req_body_json req_perform
#'   resp_check_status
#'
#' @examples
#' api <- extract_api()
#' opdefs <- extract_operation_definitions(api)
#'
#' # Generate an API function for a specific operation
#' f <- api_function_factory(opdefs[["get__files"]], api, api_key = "my_key")
#'
#' # Call the generated function with parameters (requires credentials)
#' try(
#'     result <- f(status = "value1", prefix = "value2")
#' )
#'
#' @export
api_function_factory <- function(op, api, verbosity = 0, api_key = NULL) {
    if(!is_valid_http_method(op$method)) stop("Invalid http method.")

    if (!is.numeric(verbosity) || verbosity < 0 || verbosity > 3) {
        stop("'verbosity' must be numeric between 0 and 3.")
    } else {
        verbosity <- round(verbosity)
    }

    if (!is.character(op$path) || length(op$path) > 1) {
        stop("'op$path' value must be a single character string.")
    }

    resp <- NULL # lint
    func_args <- body_exprs <- list() # will contain function arguments and body
    has_body <- !is.null(op$requestBody)
    params <- .get_operation_params(op)

    # Build the function arguments based on api operation -----
    func_args <- c(func_args, .operation_params_to_args(op))
    if (has_body) func_args <- c(func_args, pairlist2(body = ))
    # Build function body -----
    body_exprs <- c(body_exprs, .add_json_validation(op))
    url <- paste0(api$host, op$path) # Process API URL
    body_exprs <- c(body_exprs, expr(url <- !!url), .add_paths(params$path))
    req_expr <- bquote(req <- req_method(request(url), .(op$method)))
    body_exprs <- c(body_exprs, req_expr)

    if (is.null(api_key)) { # Add OAuth if API key not specified
        body_exprs <- c(body_exprs, expr(req <- ega_oauth(req)))
    } else { # otherwise modify function args
        func_args <- c(func_args, pairlist2(api_key = api_key))
    }
    body_exprs <- c(body_exprs, .add_headers(params$header, op, api, api_key))
    body_exprs <- c(body_exprs, .add_queries(params$query))
    body_exprs <- c(body_exprs, .add_request_body(has_body))
    # Perform the request and handle the response -----
    perform_req <- list(
        bquote(resp <- req_perform(req, verbosity = .(verbosity))),
        expr(resp_check_status(resp)),
        expr(result <- parse_ega_body(resp)),
        expr(return(result))
    )
    body_exprs <- c(body_exprs, perform_req)

    func_body <- expr({
        !!!body_exprs # Splice the body expression list into single expression
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
#'   host, and global settings.
#' @param ... List. List of additional arguments passed to
#'   \code{api_function_factory}
#'
#' @return A named list of functions, where each function corresponds to an API
#'   operation. The function names match the operation IDs from the
#'   specification.
#'
#' @importFrom stats setNames
#'
#' @examples
#' client <- create_client(extract_api(), api_key = "my_key", verbosity = 1)
#'
#' # Call an operation using the client (requires credentials)
#' try(
#'     result <- client$get__files(status = "value1", prefix = "value2")
#' )
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
#'   response is plain text without a JSON-like structure, a one-column tibble
#'   is returned with the raw content.
#'
#' @importFrom httr2 resp_url_path resp_body_json resp_body_string
#'   resp_content_type
#' @importFrom jsonlite fromJSON
#' @importFrom rlang :=
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#'
#' @examples
#' # Example with JSON response
#' json_resp <- httr2::response(
#'     method = "GET",
#'     url = "/api/files",
#'     status = 200,
#'     headers = list("content-type" = "application/json"),
#'     body = charToRaw('[{"id": 1, "name": "test"}]')
#' )
#' parse_ega_body(json_resp)
#'
#' # Example with plain text response
#' text_resp <- httr2::response(
#'     method = "POST",
#'     url = "/api/submissions",
#'     status = 200,
#'     headers = list("content-type" = "text/plain"),
#'     body = charToRaw("Sample response text")
#' )
#' parse_ega_body(text_resp)
#'
#' @export
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
    } else {
        err_msg <- paste(
            "Unknown content type, only 'application/json'",
            "and 'text/plain' are allowed."
        )
        stop(err_msg)
    }

    resp <- resp |>
        tibble() |>
        unnest_wider(resp, names_sep = "/", names_repair = "unique")
    # in the datasets response, there are 2 columns with status, why?

    if (ncol(resp) == 1) {
        names(resp) <- resource_name
    }

    # remove anything before slash .*/from column names
    colnames(resp) <- sub(".*\\/", "", colnames(resp))

    return(resp)
}
