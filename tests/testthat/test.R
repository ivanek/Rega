#' #' ega_parse_body
#' #' url <- "https://submission.ega-archive.org/api/files"
#' #' req <- req_method(request(url), "GET")
#' #' req <- Rega:::ega_oauth(req)
#' #' req <- req_headers(req, `Content-Type` = "application/json")
#' #' req <- req_url_query(req, status = NULL, prefix = NULL)
#' #' resp <- req_perform(req, verbosity = 3)
#' #' Rega:::ega_parse_body(resp)
#' #' }
#'
#'
#' spec_file <- system.file("extdata/ega_api_resolved.yaml", package = "Rega")
#'
#' add_required_str(c("Name", "Id", "Age"), c("Id", "Name"))
#'
#'
#' df_enum <- data.frame(key = c("A", "B"), value = c("1", "2"))
#' parse_enum(df_enum)
#'
#' vec_enum <- c("A", "B", "C")
#' parse_enum(vec_enum)
#'
#' # Filter out default ID fields
#' fields <- c("accession_id", "policy_accession_id", "name", "provisional_id")
#' filter_id_fields(fields)
#'
#' # Filter with a custom pattern
#' filter_id_fields(fields, pattern = "_id")
#'
#'
#' request_schemas <- get_schemas(extract_api())
#'
#' schemas <- get_schemas(extract_api())
#'
#' # Extract and format properties from a schema
#' get_properties(schemas[[5]])
#'
#' # Extract properties without filtering ID fields
#' get_properties(schemas[[6]], filter_ids = FALSE)
#'
#' tab <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
#' mock_endpoint <- function(id, body) list(id = id, body = body)
#' submit_table(tab, "example_id", mock_endpoint)
#'
#' responses <- list(status = "success", data = list(a = 1, b = "text"))
#' save_log(responses, logfile = NULL)
#'
#'
#' handler <- workflow_error_handler(step = "submission", responses = list(), logfile = NULL)
#' tryCatch("Example code without error", error = handler)
#' tryCatch(stop("Example code with error"), error = handler)
#'
#' # Create mock client for API endpoint
#' mock_client <- list(
#'   get__submissions__provisional_id__endpoint = function(id) {
#'     message("Mock GET request")
#'     # Simulate an empty response (no existing data)
#'     return(NULL)
#'   },
#'   post__submissions__provisional_id__endpoint = function(id, body) {
#'     message("Mock POST request")
#'     message(body) # Simulate returning submitted data
#'   }
#' )
#'
#' # Create mock data to test the function
#' test_data <- data.frame(id = 1:3, value = c("A", "B", "C"))
#'
#' # Test the function with mock data and client
#' result <- get_or_post(
#'   submission_id = 12345,
#'   data = test_data,
#'   client = mock_client,
#'   endpoint = "endpoint",
#'   retrieve_if_exists = FALSE
#' )
#'
#' # Generate a validation message
#' validation_result <- list(
#'   errors = data.frame(field = "name", error = "Missing")
#' )
#' msg <- validation_to_msg(validation_result)
#' message(msg)
#'
#' # Validate a payload against a schema
#' schema <- list(
#'   type = "object", properties = list(name = list(type = "string"))
#' )
#'
#'
#' schema = list(type = "object", list(list("string"),list("integer")))
#'
#' validate_schema(list(list("example"), list(2)), schema)
#' validate_schema(list(id = list(2), name = list(2)), schema)
#'
#'
#' opdefs = extract_operation_definitions(extract_api())
#'
#' op = get_operation_schema(opdefs[["post__submissions"]])
#'
#'
#' schema = list(
#'   type = "object",
#'   properties = list(
#'     id = list(type = "integer"),
#'     title = list(type = "string")
#'   ),
#'   required = c("id")
#' )
#'
#' payload_true = data.frame(id = c(12345), title = c("abcd"))
#' payload_false = data.frame(id = c("12345"), title = c(0.355))
#'
#' validate_schema(jsonlite::unbox(payload_true), schema)
#' validate_schema(jsonlite::unbox(payload_false), schema)
#'
#' # Get operations from API
#' opdefs = extract_operation_definitions(extract_api())
#'
#' # Retrieve the schema for a specific operation
#' schema <- get_operation_schema(opdefs[["post__submissions"]])
#'
#' opdefs <- extract_operation_definitions(extract_api())
#' opdefs[["post__submissions"]]
#'
#' # Convert operation parameters to function arguments
#' opdefs <- extract_operation_definitions(extract_api())
#'
#' Rega:::.operation_params_to_args(
#'   opdefs[["post__submissions__provisional_id__samples"]]
#' )
#'
#' Rega:::.get_operation_params(opdefs[["get__files"]])
#'
#' # Generate replacement expressions for path parameters
#' Rega:::.add_paths(c("id", "type"), "https://api/{id}/{type}")
#'
#
# opdefs <- extract_operation_definitions(extract_api())
# params = Rega:::.get_operation_params(opdefs[["get__files"]])
#
# Rega:::.add_queries(aa$query)
#
# api <- extract_api()
# opdefs <- extract_operation_definitions()
# params <- Rega:::.get_operation_params(opdefs[["get__files"]])
# Rega:::.add_headers(params$header, opdefs[["get__files"]], api)


# api <- extract_api()
# opdefs <- extract_operation_definitions(api)
# # Generate an API function for a specific operation
# f <- api_function_factory(opdefs[["get__files"]], api, api_key = "my_key")
#
# # Call the generated function with parameters (requires credentials)
# try(
#   result <- f(status = "value1", prefix = "value2")
# )
#
# library(httr2)
# fake_resp <- response(
#   status = 200,
#   headers = list("content-type" = "application/json"),
#   body = '{"key": "value"}'
# )
# parse_ega_body(fake_resp)
#
#
# # Example with JSON response
# json_resp <- httr2::response(
#   method = "GET",
#   url = "/api/files",
#   status = 200,
#   headers = list("content-type" = "application/json"),
#   body = charToRaw('[{"id": 1, "name": "test"}]')
# )
#
# resp_url_path(json_resp) |> str_replace("\\/api\\/(\\w+)\\/?.*", "\\1")
#
# resp_body_json(json_resp)
#
# parse_ega_body(json_resp)
#
# # Example with plain text response
# text_resp <- response(
#   method = "POST",
#   url = "/api/submissions",
#   status = 200,
#   headers = list("content-type" = "text/plain"),
#   body = charToRaw("Sample response text")
# )
# parse_ega_body(text_resp)


# df = data.frame(id = c("A B", "C D_"), value = c("* E F", "GH"))
# first_row_to_colnames(df, to_api = TRUE)
#
# # Load formatter params
# params <- yaml::read_yaml(system.file(
#   "extdata/default_parser_params.yaml", package = "Rega"
# ))
#
# file_formatter = get_formatter("files", params)
#
# # Dummy data, first row will be moved to column names
# tab = data.frame(
#   x1 = c("file", "value1", "value2"),
#   x2 = c("ega_inbox_relative_path", NA,  "proj1")
# )
#
# file_formatter(tab, get_formatter_params("files", params))
#
# # Load formatter params
# params <- yaml::read_yaml(system.file(
#   "extdata/default_parser_params.yaml", package = "Rega"
# ))
#
# # Dummy data, first row will be moved to column names
# tab = data.frame(
#   x1 = c("file", "value1", "value2"),
#   x2 = c("ega_inbox_relative_path", NA, "proj1")
# )
#
# file_formatter = get_formatter("files", params)
# file_formatter_params = get_formatter_params("files", params)
# file_formatter(tab, file_formatter_params)
#
# tab <- data.frame(Alias = c("name1", "name2", NA), Value = c(1, 2, 3))
# aliases_formatter(tab, params = list())
#
#
# params <- yaml::read_yaml(system.file(
#   "extdata/default_parser_params.yaml", package = "Rega"
# ))
#
# # Dummy data, first row will be moved to column names
# tab = data.frame(
#   x1 = c("file", "value1", "value2"),
#   x2 = c("ega_inbox_relative_path", NA, "proj1")
# )
#
# file_formatter(tab, get_formatter_params("files", params))
#
#
# # Load formatter params
# params <- yaml::read_yaml(system.file(
#   "extdata/default_parser_params.yaml", package = "Rega"
# ))
#
# # Dummy data, first row will be moved to column names
# tab = data.frame(
#   x1 = c("file", "value1", "value2"),
#   x2 = c("ega_inbox_relative_path", NA, "proj1")
# )
#
# ff = get_formatter("files", params)
# ff_params = get_formatter_params("files", params)
#
# ff(tab, ff_params)

# params = list(prefix = "", crypt_ext = "c4gh", prepend_slash = FALSE)
#
# params <- yaml::read_yaml(system.file(
#   "extdata/default_parser_params.yaml", package = "Rega"
# ))
#
# # Formatter parameters
# params = list(fold = "extra_attributes")
#
# # Sample data frame
# df = data.frame(
# ...1 = c("* Study", "* Title", "Extra Attributes", "Extra Attributes"),
# ...2 = c("Study1", "Title1", "A", "B"),
# ...3 = c("* Study", "* Title", "Extra Attributes", NA),
# ...4 = c("Study2", "Title2", "C", NA)
# )
#
# row_table_formatter(df, params)
#
#
# params = list(prefix = "", crypt_ext = "c4gh", prepend_slash = FALSE)
#
# # Dummy data, first row will be moved to column names
# tab = data.frame(
#   x1 = c("file", "value1", "value2"),
#   x2 = c("ega_inbox_relative_path", NA, "proj1")
# )
#
# file_formatter(tab, params)
#
#
# df = data.frame(
#     ...1 = c("* Alias", "Sample1", "Sample2"),
#     ...2 = c("* Phenotype", "wt", "ko"),
#     ...3 = c("Description", NA, NA)
# )
#
# column_table_formatter(df, list())



# Mock metadata data frame
metadata <- list(
  analyses = data.frame(
    chromosomes = I(list(
      NA,
      list("group1--1--chr1--name1", "group2--3--chr3--name3"))
    ),
    chromosome_groups = c("group1", NA),
    stringsAsFactors = FALSE
  ),
  select_input_data = list(
    chromosomes = c("group1--1--chr1--name1", "group1--2--chr2--name2"))
)

format_chromosomes(metadata)

metadata <- list(
  sheet1 = data.frame(pubmed_ids = c("123; 456", "130; 789; 102", NA))
)

process_delimited_column(metadata, "pubmed_ids", ";")


tab <- data.frame(id = c(1,2), name.1 = c("A1", NA), name.2 = c("B1", "B2"))
fold_column(tab, "name", "folded_column")

system.file("extdata/ega_full_template_v2.xlsx", package = "Rega")

try(
  default_parser(
    system.file("extdata/ega_full_template_v2.xlsx", package = "Rega")
  )
)

metadata = list(
  aliases = list(),
  files = data.frame()
)


default_validator(metadata)
