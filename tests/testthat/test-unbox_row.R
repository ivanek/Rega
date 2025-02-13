# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Unbox a single-row data frame", {
  df <- data.frame(id = 123, label = "foo", stringsAsFactors = FALSE)
  result <- unbox_row(df)

  expect_true(is.list(result))
  expect_true(all(c("id", "label") %in% names(result)))
  expect_equal(result$id, 123)
  expect_identical(result$label, "foo")
})

test_that("Unbox a single-row data frame, nested values", {
  df <- data.frame(
    id = I(list(c(123, 456))),
    label = I(list(c("foo", "bar"))),
    stringsAsFactors = FALSE
  )

  result <- unbox_row(df)

  expect_true(is.list(result))
  expect_true(all(c("id", "label") %in% names(result)))
  expect_equal(result$id, list(c(123, 456)))
  expect_equal(result$label, list(c("foo", "bar")))
})

test_that("Unbox a single-row data frame, nested data frame", {
  df <- data.frame(
    id = 1,
    label = I(list(data.frame(a = "foo", b = "bar"))),
    stringsAsFactors = FALSE
  )

  result <- unbox_row(df)

  expect_true(is.list(result))
  expect_true(all(c("id", "label") %in% names(result)))
  expect_equal(result$id, 1)
  expect_s3_class(result$label[[1]], "data.frame")
  expect_equal(names(result$label[[1]]), c("a", "b"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Unbox a multi-row data frame", {
  df <- data.frame(
    id = c(123, 456),
    label = c("foo", "bar"),
    stringsAsFactors = FALSE
  )
  expect_error(unbox_row(df), "Tried to unbox dataframe")
})

test_that("Unbox a named numeric vector of length > 1", {
  vec <- c(a = 1, b = 2)
  expect_error(unbox_row(vec), "Tried to unbox a vector")
})

test_that("Unbox a list", {
  l <- list(a = 1, b = 2)
  expect_error(unbox_row(l), "Only atomic vectors of length 1")
})

test_that("Row is NULL", {
  expect_error(unbox_row(NULL), "Only atomic vectors of length 1")
})

test_that("Row is an environment", {
  input <- new.env()
  expect_error(unbox_row(input), "No method asJSON S3")
})

test_that("Row is a function", {
  input <- function(x) x
  expect_error(unbox_row(input), "Tried to unbox a vector of length")
})
