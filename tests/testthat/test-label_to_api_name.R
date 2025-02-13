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

test_that("NULL input gets converted to zero-length vector", {
  expect_equal(label_to_api_name(NULL), character(0))
})

test_that("List of strings produces character vector,", {
  output <- label_to_api_name(list("Not", "A", "Single", "String Works"))
  expected <- c("not", "a", "single", "string_works")
  expect_equal(output, expected)
})

test_that("Numeric input", {
  expect_equal(label_to_api_name(123), "123")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------
