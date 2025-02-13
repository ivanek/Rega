# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("No security, no token, no headers => returns expression with only Content-Type", {
  header_params <- character(0)
  operation <- list(security = NULL)
  api <- list(security = NULL)

  result_expr <- Rega:::.add_headers(header_params, operation, api, token = NULL)
  result_str <- deparse(result_expr)

  expect_true(is.expression(result_expr) || is.call(result_expr))
  expect_equal(
    result_expr,
    bquote(req <- req_headers(req, `Content-Type` = "application/json"))
  )
})

test_that("Security present + token => includes Authorization header", {
  header_params <- character(0)
  operation <- list(security = TRUE)
  api <- list(security = NULL)

  result_expr <- Rega:::.add_headers(header_params, operation, api, token = "myToken")
  result_str <- deparse(result_expr)

  expect_true(is.expression(result_expr) || is.call(result_expr))
  expect_true(any(grepl("Content-Type.*application/json", result_str)))
  expect_true(any(grepl("Authorization.*paste\\(\"Bearer\", api_key\\)", result_str)))
})

test_that("Token is not NULL", {
  token <- list(key = "abcd") # is only checked for non NULL value
  operation <- list(security = TRUE)
  api <- list(security = TRUE)

  result_expr <- Rega:::.add_headers(character(0), operation, api, token = token)
  result_str <- deparse(result_expr)

  expect_true(is.expression(result_expr) || is.call(result_expr))
  expect_true(any(grepl("Authorization.*paste\\(\"Bearer\", api_key\\)", result_str)))
})

test_that("User-specified header params => include them as symbols", {
  header_params <- c("CustomHeader1", "CustomHeader2")
  operation <- list(security = NULL)
  api <- list(security = NULL)

  result_expr <- Rega:::.add_headers(header_params, operation, api, token = NULL)
  result_str <- deparse(result_expr)

  expect_true(is.expression(result_expr) || is.call(result_expr))
  expect_true(any(grepl("Content-Type.*application/json", result_str)))
  expect_true(any(grepl("CustomHeader1.*CustomHeader1", result_str)))
  expect_true(any(grepl("CustomHeader2.*CustomHeader2", result_str)))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'header_params' is not a character vector", {
  header_params <- 123
  expect_error(
    Rega:::.add_headers(header_params, list(), list()),
    "Can't convert.*to a symbol|argument is not a character vector"
  )
})

test_that("'operation' or 'api' missing fields => can't check 'security'", {
  l <- "invalid value"
  expect_error(
    Rega:::.add_headers(character(0), l, list()),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})
