# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Operation with requestBody => function has 'body' formal arg", {
  op <- list(
    method = "POST",
    path = "/items",
    requestBody = list(description = "Some body spec")
  )
  api <- list(host = "https://example.com", parameters = list())

  result_fn <- api_function_factory(op, api)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")
  args_names <- names(formals(result_fn))

  expect_type(result_fn, "closure")
  expect_true("body" %in% args_names)
  expect_match(body_text, 'url <- \"https://example.com/items\"')
  expect_match(body_text, 'req <- req_method\\(request\\(url\\), \"POST\")')
  expect_match(body_text, 'token_url = "https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token"')
})

test_that("Operation without requestBody", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  api <- list(host = "https://example.org")

  result_fn <- api_function_factory(op, api)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")
  args_names <- names(formals(result_fn))

  expect_type(result_fn, "closure")
  expect_false("body" %in% args_names)
  expect_match(body_text, 'req <- req_method\\(request\\(url\\), \"GET\")') # nolint
  expect_no_match(body_text, "req <- req_body_json\\(req, body,")
})

test_that("With api_key specified => will be present in formals", {
  op <- list(
    method = "GET",
    path = "/secret"
  )
  api <- list(host = "https://secure.example.org")

  result_fn <- api_function_factory(op, api, bearer_token = "ABCD1234")
  args_names <- names(formals(result_fn))

  expect_type(result_fn, "closure")
  expect_true("bearer_token" %in% args_names)
})

test_that("Setting verbosity level", {
  op <- list(
    method = "POST",
    path = "/items"
  )
  api <- list(host = "https://example.com")

  result_fn <- api_function_factory(op, api, verbosity = 2)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")

  expect_match(body_text, "resp <- req_perform\\(req, verbosity = 2\\)")
})

test_that("Setting token_url", {
  op <- list(
    method = "POST",
    path = "/items"
  )
  api <- list(host = "https://example.com")

  result_fn <- api_function_factory(op, api, verbosity = , token_url = "www.token.url")
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")

  expect_match(body_text, 'req <- ega_oauth\\(req, token_url = "www.token.url"\\)')
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("invalid http method", {
  op <- list(
    method = "INVALID_METHOD",
    path = "/items",
    requestBody = list(description = "Some body spec")
  )
  api <- list(host = "https://example.com", parameters = list())

  expect_error(
    api_function_factory(op, api),
    "Invalid http method"
  )
})

test_that("'api' must be a named list", {
  op <- list(
    method = "POST",
    path = "/items",
    requestBody = list(description = "Some body spec")
  )

  expect_error(api_function_factory(op, list()), "must be a named list")
  expect_error(api_function_factory(op, c()), "must be a named list")
  expect_error(api_function_factory(op, NULL), "must be a named list")
})


test_that("'op$path' is not a string", {
  op <- list(
    method = "GET",
    path = list(1, 2, 3)  # not a string
  )
  api <- list(host = "https://example.com")

  expect_error(
    api_function_factory(op, api),
    "must be a non-empty character scalar"
  )
})

test_that("'api$host' is missing or invalid", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  api <- list(openapi = "3.1.0")

  result_fn = api_function_factory(op, api)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")

  expect_type(result_fn, "closure")
  expect_match(body_text, 'url <- "/items"')
})

test_that("'verbosity is not numeric", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), verbosity = "c"),
    "'verbosity' must be numeric between 0 and 3"
  )
})

test_that("'verbosity is out of range", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), verbosity = 15),
    "'verbosity' must be numeric between 0 and 3"
  )
})

test_that("bearer_token is not character scalar or null", {
  op <- list(
    method = "GET",
    path = "/items"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), bearer_token = 123),
    "non-empty character scalar"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), bearer_token = c("foo", "bar")),
    "non-empty character scalar"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), bearer_token = list()),
    "non-empty character scalar"
  )
})

test_that("token_url token is not character scalar", {
  op <- list(
    method = "GET",
    path = "/items"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), token_url = 123),
    "non-empty character scalar"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), token_url = c("foo", "bar")),
    "non-empty character scalar"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), token_url = list()),
    "non-empty character scalar"
  )

  expect_error(
    api_function_factory(op, list(openapi = "3.1.0"), token_url = NULL),
    "non-empty character scalar"
  )
})
