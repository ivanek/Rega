# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".validate_logical_scalar accepts valid logicals", {
  expect_true(.validate_logical_scalar(TRUE))
  expect_true(.validate_logical_scalar(FALSE))

  result <- .validate_logical_scalar(TRUE)
  expect_type(result, "logical")

  expect_true(.validate_logical_scalar(TRUE, varname = "flag"))
})

test_that(".validate_logical_scalar captures the variable name correctly in errors", {
  user_setting <- "not a logical"

  expect_error(
    .validate_logical_scalar(user_setting),
    "user_setting must be a non-empty logical scalar"
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".validate_logical_scalar rejects invalid types and structures", {
  expect_error(.validate_logical_scalar(NA), "must be a non-empty logical scalar")

  expect_error(.validate_logical_scalar(1), "must be a non-empty logical scalar")
  expect_error(.validate_logical_scalar("TRUE"), "must be a non-empty logical scalar")

  expect_error(.validate_logical_scalar(c(TRUE, FALSE)), "must be a non-empty logical scalar")

  expect_error(.validate_logical_scalar(NULL), "must be a non-empty logical scalar")
})
