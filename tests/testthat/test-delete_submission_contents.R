# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("successfully deletes a submission with default client", {
  # Mock objects
  mock_response <- list(id = "12345", status = "deleted", contents = NULL)

  local_mocked_bindings(
    create_client = function(...) list(get__response = function(x) "default_client"),
    save_log = function(...) "log_saved",
    use_submission = function(id, action, client) {
      expect_equal(action, "delete")
      mock_response
    }
  )

  # Test
  result <- delete_submission_contents(id = "12345")

  expect_type(result, "list")
  expect_equal(result, mock_response)
})

test_that("works correctly when a custom client is provided", {
  mock_id <- 12345
  custom_client <- list(get__response = function(x) "custom_client")
  mock_response <- list(id = mock_id, status = "deleted", contents = NULL)

  local_mocked_bindings(
    use_submission = function(id, action, client) mock_response
  )

  expect_no_error(
    res <- delete_submission_contents(id = mock_id, client = custom_client)
  )
  expect_equal(res, mock_response)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("raises error when id is not provisional id", {
  expect_error(delete_submission_contents(id = NULL), "must be provisional ID")
  expect_error(delete_submission_contents(id = list()), "must be provisional ID")
  expect_error(delete_submission_contents(id = c("id1", "id2")), "the condition has length")
})

test_that("raises error when provided client is invalid", {
  expect_error(
    delete_submission_contents(id = 12345, client = "not_a_client"), "must be a non-empty list"
  )
  expect_error(
    delete_submission_contents(id = "12345", client = list()), "must be a non-empty list"
  )
})

test_that("propagates errors from the client method", {
  local_mocked_bindings(
    create_client = function(api) list(),
    use_submission = function(id, action, client) stop("Unauthorized Access")
  )

  expect_error(delete_submission_contents(id = "123"), "Unauthorized Access")
})
