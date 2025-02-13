# We'll define a mock endpoint function that returns a single-row data frame
# for each call, combining the input 'id' and the 'body' content:
mock_endpoint_func <- function(id, body) {
  # Turn 'body' into a data frame row (already a list from unbox_row),
  # then add 'id' column:
  data.frame(
    id = id,
    A = body$col1,
    B = body$col2,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single-row table", {
  df <- data.frame(
    col1 = "Hello",
    col2 = 999,
    stringsAsFactors = FALSE
  )

  result <- submit_table(df, id = "singleID", endpoint_func = mock_endpoint_func)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "singleID")
  expect_equal(result$A, "Hello")
  expect_equal(result$B, 999)
})


test_that("Multiple-row data frame, endpoint_func returns a data frame", {
  df <- data.frame(
    col1 = c("A", "B"),
    col2 = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- submit_table(df, id = "testID", endpoint_func = mock_endpoint_func)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c("testID", "testID"))
  expect_equal(result$A, c("A", "B"))
  expect_equal(result$B, c(1, 2))
})

test_that("Empty response", {
  df <- data.frame(col1 = c("A", "B"))
  empty_endpoint_func <- function(id, body) NULL

  result <- submit_table(df, id = "id", endpoint_func = empty_endpoint_func)
  expect_equal(result, NULL)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'tab' is not a data frame", {
  not_a_df <- list(a = 1, b = 2)

  expect_error(
    submit_table(not_a_df, id = "test", endpoint_func = mock_endpoint_func),
    "argument is of length zero"
  )
})

test_that("'endpoint_func' is not a function", {
  df <- data.frame(col1 = 1, col2 = 2)
  not_a_func <- "I am not a function"

  expect_error(
    submit_table(df, id = "test", endpoint_func = not_a_func),
    "could not find function|'endpoint_func' must be a function"
  )
})

test_that("'endpoint_func' throws an error internally", {
  df <- data.frame(col1 = c("A", "B"))
  bad_endpoint_func <- function(id, body) {
    stop("Simulated endpoint failure")
  }

  expect_error(
    submit_table(df, id = "test", endpoint_func = bad_endpoint_func),
    "Simulated endpoint failure"
  )
})

test_that("'tab' has zero rows", {
  empty_df <- data.frame(colA = character(0), colB = numeric(0))

  expect_error(
    submit_table(empty_df, id = "empty", endpoint_func = mock_endpoint_func),
    "'tab' has zero rows."
  )
})
