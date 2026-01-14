# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("it correctly counts sentences in a standard string", {
  input <- "Hello world. This is a test! Is it working?"
  result <- get_sentence_number(input)

  expect_equal(result, 3)
  expect_type(result, "integer")
})

test_that("it handles strings with no punctuation as a 'no-match' case", {
  input <- "No punctuation here"
  result <- get_sentence_number(input)

  expect_equal(result, 1)
})

test_that("it handles sentences with and without punctuation at the end", {
  input <- "Hello world. This is a test"
  result <- get_sentence_number(input)

  expect_equal(result, 2)
  expect_type(result, "integer")
})

test_that("it handles very long strings with many sentences", {
  input <- paste(rep("Word.", 100), collapse = " ")
  result <- get_sentence_number(input)

  expect_equal(result, 100)
  expect_type(result, "integer")
})

test_that("it handles character vectors", {
  input <- c(
    "Hello world. This is a test! Is it working?",
    "Lorem ipsum dolor sit amet. Consectetur adipiscing eli?",
    "Duis eu accumsan lorem"
  )
  result <- get_sentence_number(input)

  expect_equal(result, c(3, 2, 1))
  expect_type(result, "integer")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("it throws an error when input is numeric", {
  expect_error(get_sentence_number(12345), "must be a character vector")
})

test_that("it throws an error when input is a factor", {
  input <- as.factor(c("Sentence one.", "Sentence two."))
  expect_error(get_sentence_number(input), "must be a character vector")
})

test_that("it fails when passed a logical value", {
  expect_error(get_sentence_number(TRUE))
})

test_that("it fails when indexing an empty character vector", {
  empty_vec <- character(0)
  expect_error(get_sentence_number(empty_vec), "must be a character vector")
})
