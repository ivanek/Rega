# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Default schema = 'submission' matches 'EGAB' prefix and 11 digits", {
  x <- "EGAB00000000001"
  expect_true(is_accession(x))
})

test_that("Schema = 'analysis' matches 'EGAZ' prefix and 11 digits", {
  x <- "EGAZ12345678901"
  expect_true(is_accession(x, schema = "analysis"))
})

test_that("String not matching pattern", {
  bad_string <- "EGAB1234" # Not enough digits
  result <- is_accession(bad_string, "submission")
  expect_false(result)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Unknown schema triggers an error", {
  expect_error(
    is_accession("EGAB00000000001", schema = "unknown_schema"),
    "Unknown schema"
  )
})
