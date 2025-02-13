# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------
test_that("Folds multiple columns with the same prefix into a single new column", {
  df <- data.frame(
    col1_a = c("A1", NA, "A3"),
    col1_b = c(NA, "B2", "B3"),
    col2 = c("Z1", "Z2", "Z3"),
    stringsAsFactors = FALSE
  )
  result <- fold_column(df, "col1_", "folded_col")

  expect_false(any(grepl("^col1_", names(result))))
  expect_true("folded_col" %in% names(result))
  expect_equal(result$folded_col, list("A1", "B2", c("A3", "B3")))
  # col2 remains untouched
  expect_equal(result$col2, c("Z1", "Z2", "Z3"))
})

test_that("Handles empty or all-NA prefixed columns", {
  df <- data.frame(
    prefix_1 = c(NA, NA, NA),
    prefix_2 = c(NA, NA, NA),
    other = c("X1", "X2", "X3"),
    stringsAsFactors = FALSE
  )
  result <- fold_column(df, "prefix_", "folded")

  expect_false(any(grepl("^prefix_", names(result))))
  expect_true("folded" %in% names(result))
  expect_true(all(sapply(result$folded, is.na)))
  expect_equal(result$other, c("X1", "X2", "X3"))
})

test_that("No columns match the prefix", {
  # new columns is added with NA
  df <- data.frame(colA = 1:3, colB = 4:6)
  result <- fold_column(df, "prefix_", "folded")

  expect_true(all(c("colA", "colB", "folded") %in% names(result)))
  expect_true(all(sapply(result$folded, is.na)))
})

test_that("All avaiable columns are folded", {
  df <- data.frame(prefix_a = c("a", "b"), prefix_b = c("c", "d"))
  result <- fold_column(df, "p", "new_col")

  expect_equal(names(result), c("new_col"))
  expect_equal(result$new_col, list(c("a", "c"), c("b", "d")))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'tab' is not a data frame", {
  not_a_df <- 1:5
  expect_error(fold_column(not_a_df, "prefix_", "new_col"))
})

test_that("'column_prefix' is missing or empty, folds all columns", {
  df <- data.frame(prefix_a = c("a", "b"), prefix_b = c("c", "d"))
  expect_error(fold_column(df, "", "new_col"))
})

test_that("'new_name' is missing or NULL, can't rename 'tmp_fold_name'", {
  df <- data.frame(prefix_a = c("a", "b"), prefix_b = c("c", "d"))
  expect_error(fold_column(df, "prefix_", NULL))
})
