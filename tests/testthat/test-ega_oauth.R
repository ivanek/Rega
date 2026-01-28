# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------
test_that("ega_oauth creates a valid request object (Happy Paths)", {
  # Setup mock
  mock_req_auth <- function(req, client, username, password, ...) {
    req$client <- client
    req$auth <- TRUE
    req$username <- username
    req
  }

  local_mocked_bindings(
    req_oauth_password = mock_req_auth
  )

  # Tests
  input_req <- list(url = "https://api.ega.org")
  class(input_req) <- "httr2_request"

  result <- ega_oauth(
    req = input_req,
    username = "test_user",
    password = "test_password"
  )

  expect_true(result$auth)
  expect_equal(result$username, "test_user")
  expect_equal(result$url, "https://api.ega.org")
  expect_equal(
    result$client$token_url,
    "https://idp.ega-archive.org/realms/EGA/protocol/openid-connect/token"
  )

  result_custom <- ega_oauth(
    input_req,
    username = "admin",
    password = "123",
    token_url = "https://test-idp.org/token"
  )

  expect_true(result_custom$auth)
  expect_equal(result_custom$username, "admin")
  expect_equal(
    result_custom$client$token_url,
    "https://test-idp.org/token"
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("ega_oauth handles logic and type failures (Error Paths)", {
  expect_error(
    ega_oauth(req = "not a list or request"),
    "must be an 'httr2_request'"
  )
  expect_error(ega_oauth(req = 12345))

  # 2. Invalid 'token_url' type (Logic failure)
  base_req <- list(url = "https://api.ega.org")
  expect_error(
    ega_oauth(base_req, token_url = c("url1.com", "url2.com")),
    "must be a non-empty character scalar"
  )
  expect_error(ega_oauth(base_req, token_url = 100))
  expect_error(ega_oauth(new.env()))
})
