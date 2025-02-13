# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Multiple parameters, none required", {
  op <- list(
    parameters = list(
      list(name = "paramA", required = FALSE),
      list(name = "paramB")
    )
  )
  result <- Rega:::.operation_params_to_args(op)

  expect_named(result, c("paramA", "paramB"))
  expect_null(result$paramA)
  expect_null(result$paramB)
})

test_that("Mix of required and optional parameters", {
  op <- list(
    parameters = list(
      list(name = "requiredParam", required = TRUE),
      list(name = "optionalParam", required = FALSE)
    )
  )
  result <- Rega:::.operation_params_to_args(op)

  expect_named(result, c("requiredParam", "optionalParam"))
  expect_identical(result$requiredParam, quote(expr = ))
  expect_null(result$optionalParam)
})

test_that("No parameters", {
  op <- list(parameters = list())
  result <- Rega:::.operation_params_to_args(op)
  expect_equal(result, structure(list(), names = character(0)))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'op' is not a list or lacks 'parameters'", {
  op <- "invalid"
  expect_error(
    Rega:::.operation_params_to_args(op),
    "\\$ operator is invalid for atomic vectors"
  )
})

test_that("'op$parameters' is not a list => vector(...) calls fail or vapply fails", {
  op <- list(parameters = 1:3)
  expect_error(
    Rega:::.operation_params_to_args(op),
    "\\$ operator is invalid for atomic vectors"
  )
})

test_that("One of the parameters doesn't have a 'name' field", {
  op <- list(
    parameters = list(
      list(name = "goodParam"),
      list(required = TRUE)
    )
  )
  expect_error(
    Rega:::.operation_params_to_args(op),
    "values must be length 1"
  )
})
