# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Retrieves a named list from 'params$formatter[[x]][[\"params\"]]'", {
  params <- list(
    formatter = list(
      column = list(
        params = list(a = 1, b = 2)
      )
    )
  )

  result <- get_formatter_params("column", params)
  expect_equal(result, list(a = 1, b = 2))
})

test_that("'params' can be a simple vector or non-list", {
  params <- list(
    formatter = list(
      column = list(
        params = c("foo", "bar") # a simple character vector
      )
    )
  )

  result <- get_formatter_params("column", params)
  expect_equal(result, c("foo", "bar"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'params' has no 'formatter' key", {
  params <- list()
  expect_error(get_formatter_params("somecol", params))
})

test_that("'formatter' is not a list", {
  params <- list(formatter = "not_a_list")
  expect_error(get_formatter_params("somecol", params))
})

test_that("'x' not found in 'params$formatter'", {
  params <- list(
    formatter = list(
      column = list(params = list(a = 1))
    )
  )

  expect_error(get_formatter_params("unknowncol", params))
})

test_that("'params$formatter[[x]]' doesn't have 'params'", {
  # The sub-list is present, but doesn't contain "params"
  params <- list(
    formatter = list(
      column = list(type = "something")
    )
  )

  expect_error(get_formatter_params("mycol", params))
})
