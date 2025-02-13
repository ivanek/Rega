# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Multiple parameters in different categories", {
  op <- list(
    parameters = list(
      list(name = "id", `in` = "path"),
      list(name = c("sort", "prefix"), `in` = "query"),
      list(name = "authToken", `in` = "header")
    )
  )

  result <- Rega:::.get_operation_params(op)
  expect_equal(result$path, c("id"))
  expect_equal(result$query, c("sort", "prefix"))
  expect_equal(result$header, c("authToken"))
})

test_that("No parameters => returns empty character vectors", {
  op <- list(parameters = list())
  result <- Rega:::.get_operation_params(op)

  expect_equal(result$path, character(0))
  expect_equal(result$query, character(0))
  expect_equal(result$header, character(0))
})

test_that("Parameters exist but 'in' is not one of path/query/header => ignored", {
  op <- list(
    parameters = list(
      list(name = "id", `in` = "path"),
      list(name = "bodyParam", `in` = "body")
    )
  )

  result <- Rega:::.get_operation_params(op)

  expect_equal(result$path, c("id"))
  expect_equal(result$query, character(0))
  expect_equal(result$header, character(0))
})

test_that("'op' is missing 'parameters' field", {
  op <- list()
  result <- Rega:::.get_operation_params(op)
  expect_equal(result$path, character(0))
  expect_equal(result$query, character(0))
  expect_equal(result$header, character(0))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'op' is not a list => subscript or $ operator fails", {
  op <- "invalid"
  expect_error(
    Rega:::.get_operation_params(op),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})


test_that("Parameters has no 'in'", {
  op <- list(
    parameters = list(
      list(name = "id", `in` = "path"),
      list(name = "s")
    )
  )

  expect_error(
    Rega:::.get_operation_params(op),
    "argument is of length zero"
  )
})

test_that("Parameters has no 'name'", {
  op <- list(
    parameters = list(
      list(name = "id", `in` = "path"),
      list(id = 30, `in` = "query")
    )
  )

  expect_error(
    Rega:::.get_operation_params(op),
    "Parameter needs a 'name' value"
  )
})
