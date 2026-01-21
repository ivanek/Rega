#' Extract API Specification and Host Details
#'
#' This function parses an API specification file (JSON or YAML) and extracts
#' relevant details.
#'
#' @param spec_file Character. Optional.Path to the API specification file in
#'   JSON or YAML format. If NULL default `extdata/ega_api_deref.yaml` is used.
#'   Defaults to \code{NULL}
#' @param host Character. Optional. The API host URL. If not supplied, it will
#'   be inferred from the specification file's `servers` element. Defaults to
#'   \code{NULL}
#'
#' @return A list containing the parsed API specification, including the `host`
#'   and `basePath` elements. If the specification file lacks required elements,
#'   appropriate warnings or errors are raised.
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
            "extdata/ega_api_deref.yaml",
            package = "Rega"
        )
    } else {
        .validate_character_scalar(spec_file)
    }

    ext <- tolower(file_ext(spec_file))
    parse_fun <- switch(ext,
        yml = ,
        yaml = read_yaml,
        json = \(x) fromJSON(x, simplifyDataFrame = FALSE),
        stop("Specification file does not appear to be JSON or YAML.")
    )

    api <- parse_fun(spec_file)

    if (is.null(host)) {
        server_urls <- vapply(api$servers, \(x) x$url, FUN.VALUE = character(1))
        if (length(server_urls) < 1) {
            stop("Host URL not supplied and not found in specification file.")
        }
        api$host <- server_urls[1]
    } else {
        .validate_character_scalar(host)
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

    api
}

#' Extract API Operation Definitions
#'
#' This function extracts operation definitions from an API specification,
#' including HTTP methods, paths, parameters, request bodies, and responses.
#'
#' @param api List. Parsed API specification, generated from a JSON or YAML
#'   file. Must include a `paths` element with API endpoint definitions.
#'
#' @return A named list of operations, where each name corresponds to an
#'   operation ID. If operation Id is not found in the specification, unique one
#'   will be created. Each operation contains:
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
    if (!is.list(api) || is.null(names(api))) {
        stop("'api' must be a named list.")
    }

    valid_methods <- c("post", "patch", "get", "head", "delete", "put")

    operations <- list()
    paths <- api$paths
    for (path in names(paths)) {
        methods <- paths[[path]]
        for (method in intersect(names(methods), valid_methods)) {
            operation <- methods[[method]]
            operation_id <- operation$operation_id
            if (is.null(operation_id)) {
                # first remove the braces and them replace slash by
                # double underscore
                op_path <- gsub("[/]", "__", gsub("[{}]", "", path))
                op_path <- gsub("^_+", "", op_path)
                op_path <- gsub("_+$", "", op_path)

                # Generate a unique operation_id if missing
                operation_id <- paste0(
                    tolower(method),
                    "__",
                    op_path
                )

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
    operations
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
#' is_valid_http_method("GET") # TRUE
#' is_valid_http_method("get") # TRUE
#' is_valid_http_method("DELETE") # TRUE
#' is_valid_http_method("foo") # FALSE
#' is_valid_http_method(NULL) # FALSE
#' @export
is_valid_http_method <- function(m) {
    valid_methods <- c("post", "patch", "get", "head", "delete", "put")

    if (is.null(m)) {
        return(FALSE)
    }

    if (!tolower(m) %in% valid_methods) {
        return(FALSE)
    }

    TRUE
}

#' Convert Operation Parameters to Function Arguments
#'
#' This function transforms an operation's parameter definitions into a list of
#' function arguments. Required parameters are marked as missing arguments.
#'
#' @param op List. An operation definition containing a `parameters` element,
#'   which is a list of parameter definitions. Each parameter should include a
#'   `name` and an optional `required` field.
#'
#' @return A named list representing function arguments. Names correspond to
#'   parameter names, with required parameters set to missing (`quote(expr = )`)
#    and others initialized to \code{NULL}.
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
    if (!is.list(op)) {
        stop("The 'op' argument must be a list.")
    }

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
    args_list
}

#' Extract Operation Parameters by Location
#'
#' This function organizes API operation parameters into categories based on
#' their location (`path`, `query`, or `header`).
#'
#' @param op List. An operation definition containing a `parameters` element,
#'   which is a list of parameter definitions. Each parameter should include a
#'   `name` and an `in` field specifying its location.
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
    if (!is.list(op)) {
        stop("The 'op' argument must be a list.")
    }

    # Setup the output list
    params <- list(
        path = character(),
        query = character(),
        header = character()
    )

    # op$paramters can be NULL
    parameters <- op$parameters

    if (!is.null(parameters)) {
        for (p in parameters) {
            if (!is.list(p)) {
                stop("Each element in 'op$parameters' must be a list.")
            }

            if (is.null(p$name)) {
                stop("Parameter needs a 'name' value.")
            }

            # Categorize parameters
            if (p$`in` %in% c("path", "query", "header")) {
                params[[p$`in`]] <- c(params[[p$`in`]], p$name)
            }
        }
    }
    params
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
    lapply(
        path_params,
        .validate_character_scalar,
        "All 'path_params' elements"
    )

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
        rep_urls
    } else {
        list()
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
    if (!is.character(header_params)) {
        stop("The 'header_params' argument must be a character vector.")
    }

    if (!is.list(operation) || is.null(names(operation))) {
        stop("'operation' must be a named list.")
    }

    if (!is.list(api) || is.null(names(api))) {
        stop("'api' must be a named list.")
    }

    # token variable is only used to check whether api key is being passed into
    # the function
    bearer_token <- NULL # for linting
    # Add headers
    headers_list <- list(
        `Content-Type` = "application/json"
    )
    if (
        (!is.null(operation$security) || !is.null(api$security)) &&
            !is.null(token)
    ) {
        # Assuming API key authentication in header
        headers_list[["Authorization"]] <- expr(paste("Bearer", bearer_token))
    }
    if (length(header_params) > 0) {
        header_syms <- setNames(syms(header_params), header_params)
        headers_list <- c(headers_list, header_syms)
    }

    expr(req <- req_headers(req, !!!headers_list))
}

#' Generate Query Expressions for an API Request
#'
#' This function creates an expression to add query parameters to an API
#' request.
#'
#' @param query_params A character vector of query parameter names to include in
#'   the request. Query parameters are created `.get_operation_params` function.
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
    if (!is.character(query_params)) {
        stop("The 'query_params' argument must be a character vector.")
    }

    if (length(query_params) > 0) {
        query_syms <- setNames(syms(query_params), query_params)
        query_expr <- expr(
            req <- req_url_query(req, !!!query_syms)
        )
        query_expr
    } else {
        list()
    }
}

#' Add JSON Schema Validation to API Operation
#'
#' Generates validation expressions for an API operation based on its JSON
#' schema. If a schema is present, the function returns expressions to validate
#' the request body and raise an error if validation fails.
#'
#' @param op A list representing the API operation, which may contain a request
#'   body and schema.
#'
#' @return A list of expressions for JSON schema validation, or an empty list if
#'   no schema is found.
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
    if (!is.list(op) || is.null(names(op))) {
        stop("'op' must be a named list.")
    }

    has_body <- !is.null(op$requestBody)
    schema <- get_operation_schema(op)

    if (has_body && !is.null(schema)) {
        validate_expr <- expr(
            valid <- validate_schema(body, !!schema)
        )
        stop_expr <- expr(if (!valid) {
            stop(validation_to_msg(valid), call. = FALSE)
        })
        list(validate_expr, stop_expr)
    } else {
        list()
    }
}

#' Add Request Body to API Request
#'
#' Adds a JSON request body to an API request if required. If the request
#' requires a body, an expression is returned to include it; otherwise, an empty
#' list is returned.
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
    .validate_logical_scalar(has_body, "has_body")

    if (has_body) {
        expr(req <- req_body_json(req, body, auto_unbox = FALSE))
    } else {
        list()
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
#' @param bearer_token Character, optional. The API bearer token for
#'   authentication, will be included in the headers of the request. Defaults to
#'   \code{NULL}
#' @param token_url Character, optional. Token endpoint URL from which to obtain
#'   the access token. If \code{bearer_token} is specified, it will take
#'   precedence. If \code{NULL}, URL
#'   `"https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token"`
#'   will be used. Defaults to \code{NULL}.
#'
#' @return A dynamically generated function that performs the specified API
#'   operation. The function accepts arguments corresponding to operation
#'   parameters and executes the request using `httr2`.
#'
#' @importFrom rlang pairlist2 expr new_function caller_env sym !! !!!
#' @importFrom httr2 req_method request req_body_json req_perform
#'   resp_check_status
#'
#' @examples
#' api <- extract_api()
#' opdefs <- extract_operation_definitions(api)
#'
#' # Generate an API function for a specific operation
#' f <- api_function_factory(
#'     opdefs[["get__files"]], api,
#'     bearer_token = "my_key"
#' )
#'
#' # Call the generated function with parameters (requires credentials)
#' try(
#'     result <- f(status = "value1", prefix = "value2")
#' )
#'
#' @export
api_function_factory <- function(
    op, api, verbosity = 0, bearer_token = NULL, token_url = NULL
) {
    if (!is_valid_http_method(op$method)) stop("Invalid http method.")
    .validate_character_scalar(op$path)

    if (!is.list(api) || is.null(names(api))) {
        stop("'api' must be a named list.")
    }

    if (!is.numeric(verbosity) || verbosity < 0 || verbosity > 3) {
        stop("'verbosity' must be numeric between 0 and 3.")
    } else {
        verbosity <- round(verbosity)
    }

    if (!is.null(bearer_token)) {
        .validate_character_scalar(bearer_token)
    }

    if (!is.null(token_url)) {
        .validate_character_scalar(token_url)
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

    if (is.null(bearer_token)) { # Add OAuth if API key not specified
        body_exprs <- c(
            body_exprs,
            expr(req <- ega_oauth(req, token_url = !!token_url))
        )
    } else { # otherwise modify function args
        func_args <- c(func_args, pairlist2(bearer_token = bearer_token))
    }
    body_exprs <- c(
        body_exprs,
        .add_headers(params$header, op, api, bearer_token)
    )
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
    func
}

#' Generate API Client Functions
#'
#' This function creates a named list of functions for interacting with an API,
#' based on its specification and operation definitions.
#'
#' @param api List. The API specification, including operation definitions,
#'   host, and global settings.
#' @param ... List. List of additional arguments passed to
#'   \code{api_function_factory}.
#'
#' @return A named list of functions, where each function corresponds to an API
#'   operation. The function names match the operation IDs from the
#'   specification.
#'
#' @importFrom stats setNames
#'
#' @examples
#' client <- create_client(
#'     extract_api(),
#'     bearer_token = "my_key", verbosity = 1
#' )
#'
#' # Call an operation using the client (requires credentials)
#' try(
#'     result <- client$get__files(status = "value1", prefix = "value2")
#' )
#'
#' @export
create_client <- function(api, ...) {
    if (!is.list(api) || is.null(names(api))) {
        stop("'api' must be a named list.")
    }

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
#' @importFrom httr2 resp_content_type
#' @importFrom tibble is_tibble tibble
#' @importFrom tidyr unnest_wider
#'
#' @examples
#' # Example with JSON response
#' json_resp <- httr2::response(
#'     method = "GET",
#'     url = "https://www.example.com/api/files",
#'     status = 200,
#'     headers = list("content-type" = "application/json"),
#'     body = charToRaw('[{"id": 1, "name": "test"}]')
#' )
#' parse_ega_body(json_resp)
#'
#' # Example with plain text response
#' text_resp <- httr2::response(
#'     method = "POST",
#'     url = "https://www.example.com/api/submissions",
#'     status = 200,
#'     headers = list("content-type" = "text/plain"),
#'     body = charToRaw("Sample response text")
#' )
#' parse_ega_body(text_resp)
#'
#' @export
parse_ega_body <- function(resp) {
    row_data <- NULL # linter

    if (!inherits(resp, "httr2_response")) {
        stop("Argument 'resp' must be an 'httr2_response' object.")
    }

    content_type <- resp_content_type(resp)

    if (content_type == "application/json") {
        parsed_data <- parse_json_body(resp)
    } else if (content_type == "text/plain") {
        parsed_data <- parse_text_body(resp)
    } else {
        stop(
            sprintf(
                "Unknown content type '%s'. Only 'application/json' and
                'text/plain' are allowed.",
                content_type
            )
        )
    }

    # If only single json object is parsed, it needs to be wrapped in the list
    if (!is.null(names(parsed_data))) parsed_data <- list(parsed_data)

    fmt_table <- tibble(row_data = parsed_data) |>
        unnest_wider(row_data, names_sep = "/", names_repair = "unique")

    # Remove prefixes (everything before the last slash)
    names(fmt_table) <- sub(".*/", "", names(fmt_table))

    # If we ended up with a single column, set name to resource
    if (ncol(fmt_table) == 1) names(fmt_table) <- extract_resource_name(resp)

    fmt_table
}

#' Extract Resource Name from API Response URL
#'
#' Extracts the specific resource identifier (e.g., "users", "datasets") from
#' the path of an `httr2` response object by parsing the segment immediately
#' following `/api/`.
#'
#' @param resp An `httr2_response` object.
#'
#' @return A character string containing the resource name.
#'
#' @importFrom httr2 resp_url_path
#' @importFrom stringr str_replace
#'
#' @examples
#' resp <- httr2::response(
#'     method = "GET",
#'     url = "https://www.example.com/api/files"
#' )
#' extract_resource_name(resp)
#'
#' @export
extract_resource_name <- function(resp) {
    url_path <- resp_url_path(resp)
    .validate_character_scalar(url_path)
    str_replace(url_path, "\\/api\\/(\\w+)\\/?.*", "\\1")
}

#' Parse and Standardize JSON Response Body
#'
#' Extracts the JSON body from a response and ensures the output is structured
#' as a list of objects. Named lists (single records) are wrapped in a parent
#' list to maintain consistency for downstream unnesting.
#'
#' @param resp An `httr2_response` object containing JSON content.
#'
#' @return A list of lists, where each inner list represents a record.
#'
#' @importFrom httr2 resp_body_json
#'
#' @examples
#' json_resp <- httr2::response(
#'     method = "GET",
#'     url = "https://www.example.com/api/files",
#'     status = 200,
#'     headers = list("content-type" = "application/json"),
#'     body = charToRaw('[{"id": 1, "name": "test"}]')
#' )
#' parse_json_body(json_resp)
#'
#' @export
parse_json_body <- function(resp) {
    json_data <- resp_body_json(resp)

    # special treatment for single value responses (e.g. user info)
    if (!is.list(json_data)) {
        return(list(json_data))
    }

    json_data
}

#' Parse Plain Text or JSON-like Response Body
#'
#' Processes a text response by either parsing it as JSON (if structured with
#' curly braces or square brackets) or returning it as a list. Null JSON
#' elements are converted to empty lists to facilitate unnesting.
#'
#' @param resp An `httr2_response` object with "text/plain" content.
#' @param resource_name String used as the column name for raw text output.
#'
#' @return A list of parsed data or a tibble if the content is raw text.
#'
#' @importFrom httr2 resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom rlang := !!
#' @importFrom tibble tibble
#'
#' @examples
#' text_resp <- httr2::response(
#'     method = "POST",
#'     url = "https://www.example.com/api/submissions",
#'     status = 200,
#'     headers = list("content-type" = "text/plain"),
#'     body = charToRaw("Sample response text")
#' )
#' parse_text_body(text_resp, "files")
#'
#' @export
parse_text_body <- function(resp) {
    text_content <- resp_body_string(resp)

    is_json_like <- grepl("^\\{.*\\}$", text_content) ||
        grepl("^\\[.*\\]$", text_content)

    if (is_json_like) {
        parsed <- fromJSON(text_content, simplifyVector = FALSE)
        return(parsed)
    }

    text_content
}
