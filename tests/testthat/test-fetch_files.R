# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("fetch_files returns data when all files are found", {
  mock_client <- list(
    get__files = function(prefix) {
      data.frame(name = prefix, provisional_id = 12345)
    }
  )

  # 2. Mock the client creator to return our mock_client
  local_mocked_bindings(
    create_client = function(...) mock_client,
    extract_api = function() list(host = "https://example.com", parameters = list())
  )

  file_list <- list("file1.txt", "file2.csv")
  result <- fetch_files(file_list)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result$response), 2)
  expect_equal(length(result$lut), 2)
  expect_equal(result$response$name, c("file1.txt", "file2.csv"))
  expect_named(result$lut, c("file1.txt", "file2.csv"))
  expect_equal(unname(result$lut), rep(12345, 2))
})

test_that("fetch_files gives error if some files are missing or duplicated", {
  mock_client <- list(
    get__files = function(prefix) {
      if (prefix == "missing") {
        return(data.frame())
      }
      if (prefix == "duplicate") {
        return(data.frame(n = 1:2))
      }
      data.frame(n = 1)
    }
  )

  expect_error(
    fetch_files(list("file1", "missing"), client = mock_client),
    "files are missing from inbox"
  )
  expect_error(
    fetch_files(list("file1", "duplicate"), client = mock_client),
    "files are missing from inbox"
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("fetch_files throws error on empty file_list", {
  mock_client <- list(get__files = function(x) data.frame())

  expect_error(
    fetch_files(list(), client = mock_client),
    "must contain at least one element"
  )
})
