# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Simple two-column data frame, no NA", {
  df <- data.frame(
    V1 = c("Fruit", "Apple", "Banana"),
    V2 = c("Color", "Red", "Yellow"),
    stringsAsFactors = FALSE
  )
  # params argument is ignored in this function
  result <- aliases_formatter(df, params = NULL)

  expect_equal(names(result), c("fruit", "color"))
  expect_equal(result$fruit, c("Apple", "Banana"))
  expect_equal(result$color, c("Red", "Yellow"))
})

test_that("Multiple rows, some NAs get removed", {
  df <- data.frame(
    Col1 = c("Some Label", NA, "A", NA),
    Col2 = c("Another Label", "B", "C", NA),
    stringsAsFactors = FALSE
  )
  # params argument is ignored in this function
  result <- aliases_formatter(df, params = list())

  expect_equal(names(result), c("some_label", "another_label"))
  expect_equal(result$some_label, "A")
  expect_equal(result$another_label, c("B", "C"))
})

test_that("Data frame with only one row (no data rows left)", {
  single_row_df <- data.frame(
    V1 = "Header1",
    V2 = "Header2",
    stringsAsFactors = FALSE
  )
  result <- aliases_formatter(single_row_df, params = NULL)

  expect_equal(length(result), 2)
  expect_equal(result$header1, character(0))
  expect_equal(result$header2, character(0))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Input is not a data frame", {
  not_a_df <- 1:5
  expect_error(aliases_formatter(not_a_df, params = NULL))
})

test_that("Data frame with 0 rows", {
  empty_df <- data.frame()
  expect_error(aliases_formatter(empty_df, params = NULL))
})

test_that("Data frame with 0 columns", {
  no_col_df <- data.frame(row.names = c("r1", "r2"))
  expect_error(aliases_formatter(no_col_df, params = NULL))
})
