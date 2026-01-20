# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("'has_body' is TRUE", {
  result <- .add_request_body(TRUE)

  expect_true(is.call(result) || is.expression(result))
  expect_equal(result, bquote(req <- req_body_json(req, body, auto_unbox = FALSE)))
})

test_that("'has_body' is FALSE", {
  result <- .add_request_body(FALSE)
  expect_equal(result, list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'has_body' is not logical", {
  expect_error(
    .add_request_body("yes"),
    "must be a non-empty logical scalar|argument is not interpretable as logical"
  )
})

test_that("'has_body' is a vector of length > 1", {
  expect_error(
    .add_request_body(c(TRUE, FALSE)),
    "must be a non-empty logical scalar"
  )
})

test_that("'has_body' is NA", {
  expect_error(
    .add_request_body(NA),
    "must be a non-empty logical scalar"
  )
})
