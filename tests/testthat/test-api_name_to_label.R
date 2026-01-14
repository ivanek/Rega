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


test_that("List of strings produces character vector,", {
  output <- api_name_to_label(list("not", "a", "single", "string_works"))
  expected <- c("Not", "a", "Single", "String Works")
  expect_equal(output, expected)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("NULL input ", {
  expect_error(api_name_to_label(NULL), "must be a non-empty character scalar")
})


test_that("Numeric input", {
  expect_error(api_name_to_label(123), "must be a non-empty character scalar")
})

test_that("Double input", {
  expect_error(api_name_to_label(35.52), "must be a non-empty character scalar")
})

test_that("Double input", {
  expect_error(api_name_to_label(c()), "must be a non-empty character scalar")
})
