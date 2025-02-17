# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("v is TRUE ", {
  expect_match(validation_to_msg(TRUE), "No validation errors found.")
})

test_that("v is FALSE with error attributes", {
  v <- FALSE
  attr(v, "errors") <- data.frame(
    field = c("col1", "col2"),
    message = c("is missing", "is invalid"),
    stringsAsFactors = FALSE
  )

  result <- validation_to_msg(v)

  expect_match(result, "Request body raised following validation errors")
  expect_match(result, "col1.*is missing")
  expect_match(result, "col2.*is invalid")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------
test_that("v is NULL", {
  expect_error(validation_to_msg(NULL), "Validation result must be 'logical'")
})

test_that("v is not logical", {
  expect_error(
    validation_to_msg("abc"),
    "Validation result must be 'logical'"
  )

  expect_error(
    validation_to_msg(123),
    "Validation result must be 'logical'"
  )

  expect_error(
    validation_to_msg(list()),
    "Validation result must be 'logical'"
  )
})


# test_that("v is a FALSE without attributes", {
#   expect_error(
#     validation_to_msg(FALSE),
#     "Validation failed, but not error attributes are present"
#   )
# })
#
# test_that("v is a TRUE with attributes", {
#   v <- TRUE
#   attr(v, "errors") <- data.frame(
#     field = c("col1", "col2"),
#     message = c("is missing", "is invalid"),
#     stringsAsFactors = FALSE
#   )
#
#   expect_error(
#     validation_to_msg(v),
#     "Validation succeeded, but error attributes are present"
#   )
# })

test_that("v is a list with 'errors' data frame", {
  # Simulate the typical structure: v$errors is a data frame
  v_list <- list(
    errors = data.frame(
      field = c("X", "Y"),
      msg = c("Error1", "Error2"),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    validation_to_msg(v_list),
    "Validation result must be 'logical'"
  )
})

