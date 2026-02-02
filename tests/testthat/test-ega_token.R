# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("ega_token returns JSON content on success", {
  # Mock bindings
  mock_resp_success <- list(
    status_code = 200,
    body = list(access_token = "valid_token_123", expires_in = 3600)
  )

  local_mocked_bindings(
    req_perform = function(...) mock_resp_success,
    resp_body_json = function(resp) resp$body
  )

  # Test
  result <- ega_token(username = "user", password = "pwd")
  expect_type(result, "list")
  expect_equal(result$access_token, "valid_token_123")
})

test_that("ega_token handles custom token URLs", {
  custom_url <- "https://test.ega.org/token"

  local_mocked_bindings(
    req_perform = function(req) {
      # Verify the URL was correctly passed to the request
      expect_equal(req$url, custom_url)
      list(status_code = 200, body = list(token = "abc"))
    },
    resp_body_json = function(resp) resp$body
  )

  result <- ega_token(username = "a", password = "b", token_url = custom_url)
  expect_equal(result$token, "abc")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("ega_token returns error string and message on failure", {
  # Mock bindings
  mock_resp_fail <- list(
    status_code = 401,
    body_str = "Unauthorized access"
  )

  local_mocked_bindings(
    req_perform = function(...) mock_resp_fail,
    resp_body_string = function(resp) resp$body_str
  )

  # Test
  expect_message(
    result <- ega_token(),
    "Failed to obtain token: 401"
  )

  expect_equal(result, "Unauthorized access")
  expect_type(result, "character")
})

test_that("ega_token rejects invalid token_url types ", {
  expect_error(ega_token(token_url = 123), "non-empty character scalar")
  expect_error(
    ega_token(token_url = c("url1", "url2")),
    "non-empty character scalar"
  )

  expect_error(
    ega_token(base_req, token_url = NULL),
    "must be a non-empty character scalar"
  )

  expect_error(
    ega_token(base_req, token_url = list()),
    "must be a non-empty character scalar"
  )
})
