# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("extract_resource_name handles happy paths correctly", {
  local_mocked_bindings(
    resp_url_path = function(resp) resp$url_path
  )

  resp_std <- list(url_path = "/api/datasets/EGAD0001")
  expect_equal(extract_resource_name(resp_std), "datasets")
  expect_type(extract_resource_name(resp_std), "character")

  resp_slash <- list(url_path = "/api/users/?query=test")
  expect_equal(extract_resource_name(resp_slash), "users")

  resp_simple <- list(url_path = "/api/files")
  expect_equal(extract_resource_name(resp_simple), "files")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("extract_resource_name handles error paths and malformed paths", {
  local_mocked_bindings(
    resp_url_path = function(resp) resp$url_path
  )

  resp_no_api <- list(url_path = "/v1/datasets")
  expect_equal(extract_resource_name(resp_no_api), "/v1/datasets")

  resp_empty <- list(url_path = "/api//")
  expect_equal(extract_resource_name(resp_empty), "/api//")

  resp_num <- list(url_path = 12345)
  expect_error(extract_resource_name(resp_num), "must be a non-empty character scalar")

  expect_error(extract_resource_name(NULL), "must be a non-empty character scalar")
  expect_error(extract_resource_name(list()), "must be a non-empty character scalar")
  expect_error(extract_resource_name(c()), "must be a non-empty character scalar")
})
