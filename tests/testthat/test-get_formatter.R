# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------
test_that("Retrieves a base R function by name", {
  params <- list(
    formatter = list(
      column = list(type = "mean")
    )
  )
  result <- get_formatter("column", params)

  expect_identical(result, mean)
})

test_that("Retrieves a user-defined function", {
  column_formatter <- function(x) paste("Special:", x)
  params <- list(
    formatter = list(
      sample_data = list(type = "column_formatter")
    )
  )
  result <- get_formatter("sample_data", params)

  expect_true(is.function(result))
  expect_equal(result("test"), column_formatter("test"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'x' not present in params$formatter", {
  params <- list(
    formatter = list(
      column = list(type = "mean")
    )
  )

  expect_error(get_formatter("row", params))
})

test_that("'type' element is missing from the relevant formatter list", {
  params <- list(
    formatter = list(
      column = list() # 'type' is missing
    )
  )

  expect_error(get_formatter("column", params))
})

test_that("Function specified by 'type' doesn't exist in the environment", {
  params <- list(
    formatter = list(
      column = list(type = "this_function_does_not_exist")
    )
  )

  expect_error(
    get_formatter("column", params),
    "object 'this_function_does_not_exist' not found"
  )
})

test_that("'params$formatter' is not defined or not a list structure", {
  params <- list()
  expect_error(get_formatter("column", params))
})
