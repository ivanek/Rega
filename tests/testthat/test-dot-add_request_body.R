# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("'has_body' is TRUE", {
  result <- Rega:::.add_request_body(TRUE)

  expect_true(is.call(result) || is.expression(result))
  expect_equal(result, bquote(req <- req_body_json(req, body, auto_unbox = FALSE)))
})

test_that("'has_body' is FALSE", {
  result <- Rega:::.add_request_body(FALSE)
  expect_equal(result, list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'has_body' is not logical", {
  expect_error(
    Rega:::.add_request_body("yes"),
    "'has_body' must be logical|argument is not interpretable as logical"
  )
})

test_that("'has_body' is a vector of length > 1", {
  expect_error(
    Rega:::.add_request_body(c(TRUE, FALSE)),
    "the condition has length > 1"
  )
})

test_that("'has_body' is NA", {
  expect_error(
    Rega:::.add_request_body(NA),
    "missing value where TRUE/FALSE needed"
  )
})
