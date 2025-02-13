# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single query param => returns an expression", {
  params <- c("search")
  result <- Rega:::.add_queries(params)

  expect_true(is.call(result) || is.expression(result))
  expect_equal(result, bquote(req <- req_url_query(req, search = search)))
})

test_that("Multiple query params", {
  params <- c("page", "limit")
  result <- Rega:::.add_queries(params)

  expect_true(is.call(result) || is.expression(result))
  expect_equal(
    result,
    bquote(req <- req_url_query(req, page = page, limit = limit))
  )
})

test_that("No query params", {
  result <- Rega:::.add_queries(character(0))
  expect_equal(result, list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'query_params' is not a character vector", {
  params <- 123
  expect_error(
    Rega:::.add_queries(params),
    "Can't convert.*to a symbol"
  )
})

test_that("One element of 'query_params' is non-character => fails in syms()", {
  params <- list("validParam", 99)
  expect_error(
    Rega:::.add_queries(params),
    "Can't convert.*to a symbol"
  )
})
