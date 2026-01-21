# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("finalise_submission processes valid accession and provisional IDs", {
  # Mocks
  local_mocked_bindings(
    ega_oauth = function(...) TRUE,
    parse_ega_body = function(...) "success",

    req_method = function(...) TRUE,
    req_headers = function(...) TRUE,
    req_body_json = function(...) TRUE,
    req_perform = function(...) TRUE,
    resp_check_status = function(...) TRUE
  )

  # Tests
  result1 <- finalise_submission(id = "EGAB00000000001", release_date = "2026-12-31")
  expect_equal(result1, "success")

  result2 <- finalise_submission(id = "123456", release_date = "2025-01-31")
  expect_equal(result2, "success")
})

test_that("finalise_submission handles dataset_changelogs correctly", {
  local_mocked_bindings(
    ega_oauth = function(...) TRUE,
    parse_ega_body = function(...) changelog_df,

    req_method = function(...) TRUE,
    req_headers = function(...) TRUE,
    req_body_json = function(...) TRUE,
    req_perform = function(...) TRUE,
    resp_check_status = function(...) TRUE
  )

  changelog_df <- data.frame(
    dataset = c("EGAD00000000001", "EGAD00000000002"),
    message = c("Initial", "Update"),
    stringsAsFactors = FALSE
  )

  result <- finalise_submission("12345", "2026-01-01", changelog_df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("finalise_submission throws errors for invalid IDs and Dates", {
  # Invalid ID type
  expect_error(
    finalise_submission("INVALID_ID", "2026-01-01"),
    "Unknown ID type"
  )

  expect_error(
    finalise_submission(c(), "2026-01-01"),
    "Unknown ID type"
  )

  # Incorrect date format
  expect_error(
    finalise_submission("12345", "31-12-2026"),
    "Incorrect 'release_date' format"
  )

  expect_error(
    finalise_submission("12345", "aaa"),
    "Incorrect 'release_date' format"
  )

  expect_error(
    finalise_submission("12345", list()),
    "must be a non-empty character scalar"
  )

  expect_error(
    finalise_submission("12345", 12345),
    "must be a non-empty character scalar"
  )
})

test_that("finalise_submission throws errors for invalid dataset_changelog", {
  expect_error(
    finalise_submission(
      "12345", "2026-01-01",
      dataset_changelogs = data.frame(dataset = "DS1", other = "text")
    ),
    "must contain following columns"
  )

  expect_error(
    finalise_submission(
      "12345", "2026-01-01",
      dataset_changelogs = data.frame(studies = "DS1", message = "text")
    ),
    "must contain following columns"
  )

  expect_error(
    finalise_submission(
      "12345", "2026-01-01",
      dataset_changelogs = data.frame(dataset = 123, message = "text")
    ),
    "'dataset_changelogs' columns must be character type without NA"
  )

  expect_error(
    finalise_submission(
      "12345", "2026-01-01",
      dataset_changelogs = data.frame(dataset = "123", message = TRUE)
    ),
    "'dataset_changelogs' columns must be character type without NA"
  )

  expect_error(
    finalise_submission(
      "12345", "2026-01-01",
      dataset_changelogs = data.frame(dataset = NA_character_, message = "TRUE")
    ),
    "'dataset_changelogs' columns must be character type without NA"
  )
})

test_that("raises error when provided client is invalid", {
  expect_error(
    finalise_submission(
      id = 12345, release_date = "2025-01-01", client = "not_a_client"
    ),
    "must be a non-empty list"
  )

  expect_error(
    finalise_submission(
      id = "12345", release_date = "2025-01-01", client = list()
    ),
    "must be a non-empty list"
  )
})
