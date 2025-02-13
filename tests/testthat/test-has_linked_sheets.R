# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Some data frames have the column with non-NA, some don't", {
  df1 <- data.frame(a = c(1, 2), b = c(NA, NA))
  df2 <- data.frame(a = c(NA, 3), b = c("X", "Y"))
  df3 <- data.frame(c = 1:2) # missing column 'a'

  metadata <- list(df1, df2, df3)
  result <- has_linked_sheets(metadata, "a")

  expect_equal(result, c(TRUE, TRUE, FALSE))
})

test_that("Multiple data frames, all NA in one, partial NA in another, missing column in another", {
  df1 <- data.frame(a = c(NA, NA), x = c("foo", "bar"))
  df2 <- data.frame(a = c(1, NA), y = c("alpha", "beta"))
  df3 <- data.frame(b = 1:3)

  metadata <- list(df1, df2, df3)
  result <- has_linked_sheets(metadata, "a")
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("'metadata' is an empty list", {
  expect_equal(has_linked_sheets(list(), "a"), logical(0))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'metadata' is not a list", {
  not_a_list <- data.frame(a = 1:2)
  expect_error(has_linked_sheets(not_a_list, "a"))
})

test_that("'colname' is not a string", {
  df1 <- data.frame(a = 1:2)
  metadata <- list(df1)
  colname <- 123

  expect_error(has_linked_sheets(metadata, colname))
})
