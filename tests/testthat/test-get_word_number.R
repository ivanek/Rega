# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("it correctly counts words in a standard sentence", {
  input <- "The quick brown fox"
  result <- get_word_number(input)

  expect_equal(result, 4)
  expect_type(result, "integer")
})

test_that("it handles vectors of multiple strings", {
  input <- c("Hello world", "Test", "One two three")
  result <- get_word_number(input)

  expect_equal(result, c(2, 1, 3))
  expect_type(result, "integer")
})

test_that("it handles boundary cases like empty strings or single characters", {
  expect_equal(get_word_number(""), 0)
  expect_equal(get_word_number("A"), 1)
  expect_equal(get_word_number("   "), 0)
  expect_equal(get_word_number(c("   ", "A", "BB", "C C")), c(0, 1, 1, 2))
  expect_equal(get_word_number(c("", NULL)), c(0))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("it throws an error when input is not a character vector", {
  expect_error(get_word_number(123), "'text' must be a character vector.")
  expect_error(get_word_number(TRUE), "'text' must be a character vector.")
  expect_error(get_word_number(list("text")), "'text' must be a character vector.")
})

test_that("it fails when passed a data frame or matrix", {
  input_df <- data.frame(text = c("hello", "world"))
  expect_error(get_word_number(input_df))
})

test_that("it fails gracefully with NULL input", {
  expect_error(get_word_number(NULL))
})
