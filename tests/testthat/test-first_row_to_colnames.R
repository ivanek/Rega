# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("First row is used as column names (to_api = TRUE)", {
  # Example data frame
  df <- data.frame(
    col1 = c("Name", "John"),
    col2 = c("Age", 21)
  )

  result <- first_row_to_colnames(df, to_api = TRUE)

  expect_equal(names(result), c("name", "age"))
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "John")
  expect_equal(result$age, "21")
})

test_that("First row is used as column names (to_api = FALSE)", {
  # If to_api = FALSE, we skip label_to_api_name() and just use make.names()
  # directly on the first row.
  df <- data.frame(
    x1 = c("My Label", "Foo"),
    x2 = c("Another Label", "Bar")
  )

  result <- first_row_to_colnames(df, to_api = FALSE)

  expect_equal(names(result), c("My.Label", "Another.Label"))
  expect_equal(nrow(result), 1)
  expect_equal(result$My.Label, "Foo")
  expect_equal(result$Another.Label, "Bar")
})

test_that("Empty data frame", {
  empty_df <- data.frame()
  expect_equal(nrow(first_row_to_colnames(empty_df)), 0)
  expect_equal(ncol(first_row_to_colnames(empty_df)), 0)
  expect_equal(names(first_row_to_colnames(empty_df)), character(0))
})

test_that("Data frame with only 1 row", {
  single_row_df <- data.frame(a = "HeaderOnly", b = "Header2")
  expect_equal(nrow(first_row_to_colnames(single_row_df)), 0)
  expect_equal(ncol(first_row_to_colnames(single_row_df)), 2)
  expect_equal(
    names(first_row_to_colnames(single_row_df)),
    c("headeronly", "header2")
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Non-data-frame input", {
  not_a_df <- 1:5
  expect_error(first_row_to_colnames(not_a_df))
})
