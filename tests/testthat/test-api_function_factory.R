# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Operation with requestBody => function has 'body' formal arg", {
  op <- list(
    method = "POST",
    path = "/items",
    requestBody = list(description = "Some body spec")
  )
  api <- list(host = "https://example.com")

  result_fn <- api_function_factory(op, api)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")
  args_names <- names(formals(result_fn))

  expect_type(result_fn, "closure")
  expect_true("body" %in% args_names)
  expect_match(body_text, 'url <- \"https://example.com/items\"')
  expect_match(body_text, 'req <- req_method\\(request\\(url\\), \"POST\")')
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
  expect_match(body_text, 'req <- req_method\\(request\\(url\\), \"GET\")')
  expect_no_match(body_text, 'req <- req_body_json\\(req, body,')
})

test_that("With api_key specified => will be present in formals", {
  op <- list(
    method = "GET",
    path = "/secret"
  )
  api <- list(host = "https://secure.example.org")

  result_fn <- api_function_factory(op, api, api_key="ABCD1234")
  args_names <- names(formals(result_fn))

  expect_type(result_fn, "closure")
  expect_true("api_key" %in% args_names)
})

test_that("Setting verbosity level", {
  op <- list(
    method = "POST",
    path = "/items"
  )
  api <- list(host = "https://example.com")

  result_fn <- api_function_factory(op, api, verbosity = 2)
  body_text <- paste(deparse(rlang::fn_body(result_fn)), collapse = " ")

  expect_match(body_text, 'resp <- req_perform\\(req, verbosity = 2\\)')
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'op$path' is not a string", {
  op <- list(
    method = "GET",
    path = list(1, 2, 3)  # not a string
  )
  api <- list(host = "https://example.com")

  expect_error(
    api_function_factory(op, api),
    "value must be a single character string"
  )
})

test_that("'api$host' is missing or invalid", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  api <- list()

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
    api_function_factory(op, list(), verbosity = "c"),
    "'verbosity' must be numeric between 0 and 3"
  )
})

test_that("'verbosity is out of range", {
  op <- list(
    method = "GET",
    path = "/items"
  )
  expect_error(
    api_function_factory(op, list(), verbosity = 15),
    "'verbosity' must be numeric between 0 and 3"
  )
})
