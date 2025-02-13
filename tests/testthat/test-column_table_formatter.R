# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Basic formatting without 'fold' parameter", {
  # also trims values
  df <- data.frame(
    V1 = c("Name", " Alice ", "  Bob  "),
    V2 = c("Age", " 30", "40  "),
    stringsAsFactors = FALSE
  )
  result <- column_table_formatter(df, params = NULL)

  expect_equal(colnames(result), c("name", "age"))
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("Alice", "Bob"))
  expect_equal(result$age, c("30", "40"))
})

test_that("'fold' parameter used to fold a column", {
  df <- data.frame(
    V1 = c("OldCol", "A ", "B"),
    V2 = c("Value", " Val1", "Val2"),
    V3 = c("Value.1", " Val3", "Val4"),
    V4 = c("Value.2", " Val5", "Val6"),
    stringsAsFactors = FALSE
  )

  params <- list(fold = c("Value"))
  result <- column_table_formatter(df, params = params)

  expect_equal(names(result), c("oldcol", "value"))
  expect_equal(result$oldcol, c("A", "B"))
  expect_equal(result$value, list(
    c("Val1", "Val3", "Val5"),
    c("Val2", "Val4", "Val6")
  ))
})

test_that("NA removal", {
  df <- data.frame(
    V1 = c("Name", " Alice ", "  Bob  ", NA),
    V2 = c("Age", " 30", "40  ", NA),
    V3 = c("Status", NA, "married", NA),
    stringsAsFactors = FALSE
  )
  result <- column_table_formatter(df, params = list())

  expect_equal(colnames(result), c("name", "age", "status"))
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("Alice", "Bob"))
  expect_equal(result$age, c("30", "40"))
  expect_equal(result$status, c(NA, "married"))
})

test_that("Data frame with 0 rows", {
  result <- column_table_formatter(data.frame(), params = NULL)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
  expect_equal(names(result), character(0))
})

test_that("Data frame with 0 columns", {
  result <- column_table_formatter(
    data.frame(row.names = c("r1", "r2")),
    params = NULL
  )

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
  expect_equal(names(result), character(0))
})

test_that("Data frame with only one row (no data rows left)", {
  single_row_df <- data.frame(
    V1 = "Header1",
    V2 = "Header2",
    stringsAsFactors = FALSE
  )
  result <- column_table_formatter(single_row_df, params = NULL)

  expect_equal(length(result), 2)
  expect_equal(result$header1, character(0))
  expect_equal(result$header2, character(0))
})

test_that("'fold' column in params not present in the data.", {
  df <- data.frame(
    V1 = c("Col1", "X", "Y"),
    V2 = c("Col2", "1", "2"),
    stringsAsFactors = FALSE
  )
  params <- list(fold = c("NonExistent"))
  result <- column_table_formatter(df, params = params)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("col1", "col2", "nonexistent"))
  expect_equal(result$col1, c("X", "Y"))
  expect_equal(result$col2, c("1", "2"))
  expect_equal(result$nonexistent, c(list(NA_character_), list(NA_character_)))
})

test_that("'params' correct format", {
  df <- data.frame(
    V1 = c("Name", "Alice"),
    V2 = c("Age", "30"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    dim(column_table_formatter(df, params = FALSE)), c(1, 2)
  )
  expect_equal(
    dim(column_table_formatter(df, params = list())), c(1, 2)
  )
  expect_equal(
    dim(column_table_formatter(df, params = list(value = "foo"))), c(1, 2)
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Non-data-frame input", {
  not_a_df <- 1:5
  expect_error(column_table_formatter(not_a_df, params = NULL))
})

test_that("'params' is wrong format", {
  df <- data.frame(
    V1 = c("Name", "Alice"),
    V2 = c("Age", "30"),
    stringsAsFactors = FALSE
  )

  expect_error(column_table_formatter(df, params = "not a list"))
  expect_error(column_table_formatter(df, params = TRUE))
  expect_error(column_table_formatter(df, params = 123))
  expect_error(column_table_formatter(df, params = 0))
  expect_error(column_table_formatter(df, params = c(1,2,3)))
})
