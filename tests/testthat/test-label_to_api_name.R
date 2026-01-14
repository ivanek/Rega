# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Removes leading '* '", {
  output <- label_to_api_name("* Hello World")
  expect_equal(output, "hello_world")
})

test_that("Happy path: Replaces spaces with underscores, converts to lowercase", {
  output <- label_to_api_name("My Label")
  expect_equal(output, "my_label")
})

test_that("List of strings produces character vector,", {
  output <- label_to_api_name(list("Not", "A", "Single", "String Works"))
  expected <- c("not", "a", "single", "string_works")
  expect_equal(output, expected)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("NULL input triggers error", {
  expect_error(label_to_api_name(NULL), "must be a character vector")
})

test_that("Numeric input", {
  expect_error(label_to_api_name(123), "must be a character vector")
})
