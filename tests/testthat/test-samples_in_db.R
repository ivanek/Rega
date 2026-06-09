# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("samples_in_db works on happy paths", {
  # Setup mock client
  mock_client <- list(
    get__samples = function() data.frame(alias = c("S1", "S2"))
  )

  expect_false(samples_in_db(c("S3", "S4"), client = mock_client))
  expect_type(samples_in_db("S3", client = mock_client), "logical")

  expect_false(
    samples_in_db(c("S1", "S3"), client = mock_client, retrieve = TRUE)
  )

  expect_error(
    samples_in_db(c("S1", "S3"), client = mock_client, retrieve = FALSE)
  )

  long_vec <- paste0("sample_", 1:100)
  expect_false(samples_in_db(long_vec, client = mock_client))
})

test_that("samples_in_db default client path", {
  mock_client <- list(
    get__samples = function() data.frame(alias = c("S1", "S2"))
  )

  local_mocked_bindings(
    create_client = function(...) mock_client
  )

  expect_false(samples_in_db(c("S3", "S4")))
  expect_type(samples_in_db("S3"), "logical")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("samples_in_db throws expected errors", {
  # Setup mock client
  mock_client <- list(
    get__samples = function() data.frame(alias = c("S1", "S2"))
  )

  expect_error(
    samples_in_db(c("S1", "S5"), client = mock_client, retrieve = FALSE),
    regexp = "Samples aliases per submitter must be unique"
  )

  expect_error(
    samples_in_db(c(1, 2, 3), client = mock_client),
    regexp = "must be a character vector"
  )

  expect_error(
    samples_in_db(character(0), client = mock_client),
    regexp = "at least one element"
  )

  expect_error(
    samples_in_db("S3", client = mock_client, retrieve = "not_logical"),
    regexp = "must be a non-empty logical scalar"
  )
})
