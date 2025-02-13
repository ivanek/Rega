# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Some strings match pattern, some don't => only non-matching remain", {
  input <- c("accession_id", "my_field", "policy_accession_id", "provisional_id")
  result <- filter_id_fields(input)
  expect_equal(result, c("my_field", "policy_accession_id"))
})

test_that("No strings match => everything remains", {
  input <- c("field1", "another_field", "policy_accessionXYZ")
  result <- filter_id_fields(input)
  expect_equal(result, input)
})

test_that("All strings match => empty vector returned", {
  input <- c("accession_id", "some_accession_id", "provisional_id")
  result <- filter_id_fields(input)
  expect_length(result, 0)
})

test_that("'x' is an empty character vector => returns empty", {
  empty_input <- character(0)
  result <- filter_id_fields(empty_input)
  expect_equal(result, character(0))
})


test_that("Custom pattern", {
  input <- c("custom_id", "some_accession_id", "provisional_id")
  result <- filter_id_fields(input, "custom")
  expect_equal(result, c("some_accession_id", "provisional_id"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'x' is not a character vector", {
  numeric_x <- c(1, 2, 3)
  expect_error(
    filter_id_fields(numeric_x),
    "'x' must be a character vector"
  )
})

test_that("Invalid regex in 'pattern' => grepl fails", {
  bad_pattern <- "(*invalidregex"

  input <- c("accession_id", "my_field")
  expect_warning(
    expect_error(
      filter_id_fields(input, pattern = bad_pattern),
      "invalid regular expression|unrecognized character"
    ), "PCRE pattern compilation error"
  )
})
