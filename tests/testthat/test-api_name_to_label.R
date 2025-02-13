# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single string with underscores", {
  output <- api_name_to_label("hello_world")
  expect_equal(output, "Hello World")
})

test_that("Single string without underscores", {
  output <- api_name_to_label("api")
  expect_equal(output, "Api")
})

test_that("NULL input gets converted to zero-length vector", {
  expect_equal(api_name_to_label(NULL), character(0))
})

test_that("List of strings produces character vector,", {
  output <- api_name_to_label(list("not", "a", "single", "string_works"))
  expected <- c("Not", "a", "Single", "String Works")
  expect_equal(output, expected)
})

test_that("Numeric input", {
  expect_equal(api_name_to_label(123), "123")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------
