# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Splits a regular delimited column and trims whitespace", {
  df <- data.frame(
    title = c("Some Author", "Another Author"),
    authors = c("Alice , Bob", "   Charlie,Deb  "),
    stringsAsFactors = FALSE
  )
  metadata <- list(sheet1 = df)

  result <- process_delimited_column(
    metadata,
    column_name = "authors",
    separator = ","
  )

  expect_type(result$sheet1$authors, "list")
  expect_equal(result$sheet1$authors[[1]], c("Alice", "Bob"))
  expect_equal(result$sheet1$authors[[2]], c("Charlie", "Deb"))
})

test_that("'pubmed_ids' column is converted to integer", {
  df <- data.frame(
    pubmed_ids = c("  123456,  789", NA, "1001  "),
    other_col = c("foo", "bar", "baz"),
    stringsAsFactors = FALSE
  )
  metadata <- list(pub_sheet = df)

  result <- process_delimited_column(metadata, "pubmed_ids", separator = ",")

  expect_true("pub_sheet" %in% names(result))
  expect_s3_class(result$pub_sheet, "data.frame")
  pmids <- result$pub_sheet$pubmed_ids
  expect_type(pmids, "list")

  expect_equal(pmids[[1]], c(123456L, 789L))
  expect_equal(pmids[[2]], list())
  expect_equal(pmids[[3]], 1001L)
})

test_that("'metadata' is not a list", {
  # If metadata is not a list of data frame, column will not be found and
  # evaluation will be skipped
  not_a_list <- data.frame(column_name = c("A,B"))

  expect_equal(
    process_delimited_column(not_a_list, "column_name", separator = ","),
    not_a_list
  )
})


test_that("'column_name' not found in any sheet", {
  # Silently skipped
  df <- data.frame(some_col = c("Val1,Val2"), stringsAsFactors = FALSE)
  metadata <- list(sheet1 = df)

  expect_equal(
    process_delimited_column(metadata, "nonexistent_col", separator = ","),
    metadata
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("An element in 'metadata' is not a data frame", {
  # 'metadata' is a list, but one element is a vector instead of a data frame
  # it will be skipped
  bad_element <- c("A,B", "C,D")
  good_df <- data.frame(column_name = c("X , Y", "Z"), stringsAsFactors = FALSE)
  metadata <- list(sheet1 = good_df, sheet2 = bad_element)
  result <- process_delimited_column(metadata, "column_name", separator = ",")

  expect_equal(result$sheet1$column_name, list(c("X", "Y"), c("Z")))
  expect_equal(result$sheet2, c("A,B", "C,D"))
})


test_that("Invalid separator (e.g., NULL)", {
  df <- data.frame(my_col = c("A,B,C"), stringsAsFactors = FALSE)
  metadata <- list(sheet1 = df)

  expect_error(
    process_delimited_column(metadata, "my_col", separator = NULL),
    "`pattern` must be a string"
  )
})
