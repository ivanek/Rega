# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("successfully deletes a submission with default client", {
  # Mock objects
  mock_response <- list(status = "success", id = "14345")
  mock_client_obj <- list(
    delete__submissions__provisional_id = function(id) mock_response
  )

  local_mocked_bindings(
    create_client = function(...) mock_client_obj,
    save_log = function(...) "log_saved",
    .package = "Rega"
  )

  # Test
  result <- delete_submission(id = "14345")

  expect_type(result, "list")
  expect_named(result, "submission")
  expect_equal(result$submission$id, "14345")
})

test_that("works correctly when a custom client is provided", {
  mock_response <- list(status = "deleted")
  custom_client <- list(
    delete__submissions__provisional_id = function(id) mock_response
  )

  local_mocked_bindings(
    .is_client = function(x) TRUE,
    create_client = function(...) stop("Should not be called")
  )

  result <- delete_submission(id = "12345", client = custom_client)

  expect_equal(result$submission$status, "deleted")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("raises error when id is not a character scalar", {
  expect_error(delete_submission(id = NULL), "ID must be provisional ID")
  expect_error(delete_submission(id = list()), "ID must be provisional ID")
  expect_error(delete_submission(id = c("id1", "id2")), "condition has length > 1")
})

test_that("raises error when provided client is invalid", {
  expect_error(
    delete_submission(id = 12345, client = "not_a_client"), "must be a non-empty list"
  )
  expect_error(
    delete_submission(id = "12345", client = list()), "must be a non-empty list"
  )
})

test_that("propagates errors from the client method", {
  mock_client_obj <- list(
    delete__submissions__provisional_id = function(id) stop("API Connection Timeout")
  )

  local_mocked_bindings(
    create_client = function(...) mock_client_obj
  )

  expect_error(delete_submission(id = "12345"), "API Connection Timeout")
})
