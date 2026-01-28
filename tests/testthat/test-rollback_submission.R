# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("handles a single endpoint and returns a list", {
  # Mock objects
  mock_client <- list(
    put__submissions__accession_id__analysis__rollback = function(id) paste("rolled back", id, "analysis")
  )

  # Test
  result <- rollback_submission(
    id = "EGAB00000000001",
    endpoints = list("analysis"),
    client = mock_client
  )

  expect_type(result, "list")
  expect_equal(result[[1]], "rolled back EGAB00000000001 analysis")
})

test_that("successfully rolls back multiple endpoints with a default client", {
  # Mock objects
  mock_client <- list(
    put__submissions__accession_id__samples__rollback = function(id) paste("rolled back", id, "samples"),
    put__submissions__accession_id__files__rollback = function(id) paste("rolled back", id, "files")
  )

  local_mocked_bindings(
    create_client = function(api) mock_client,
    save_log = function(...) "log_saved"
  )

  # Test
  result <- rollback_submission(
    id = "EGAB00000000001",
    endpoints = list("samples", "files")
  )

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result[[1]], "rolled back EGAB00000000001 samples")
  expect_equal(result[[2]], "rolled back EGAB00000000001 files")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("throws error when 'id' is not correct EGA accession ID", {
  expect_error(
    rollback_submission(id = 123, endpoints = list("samples", "files")),
    "Incorrect format of accesssion ID"
  )
  expect_error(
    rollback_submission(id = NULL, endpoints = list("samples", "files")),
    "Incorrect format of accesssion ID"
  )
  expect_error(
    rollback_submission(id = c("aa", "bb"), endpoints = list("samples", "files")),
    "condition has length > 1"
  )
})

test_that("throws error when 'endpoints' is not a list", {
  expect_error(
    rollback_submission(id = "EGAB00000000001", endpoints = c("samples", "files")),
    "must be a list"
  )
})

test_that("throws error if endpoint elements are not character scalars", {
  expect_error(
    rollback_submission(id = "EGAB00000000001", endpoints = list("samples", 123)),
    "All elements in endpoints"
  )
  expect_error(
    rollback_submission(id = "EGAB00000000001", endpoints = list(NULL)),
    "All elements in endpoints"
  )
})

test_that("throws error if is_accession validation fails", {
  expect_error(
    rollback_submission(id = "INVALID_ID", endpoints = list("samples")),
    "Incorrect format of accesssion ID"
  )
})

test_that("fails if the client does not have the dynamically constructed method", {
  # Mock objects
  mock_client <- list(
    get__some_other_method = function(x) x,
    put__incorrect_rollback = function(x) x
  )

  # Test
  expect_error(
    rollback_submission(
      id = "EGAB00000000001",
      endpoints = list("analysis"),
      client = mock_client
    ),
    "attempt to apply non-function"
  )
})
