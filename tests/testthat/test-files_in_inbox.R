# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("files_in_inbox returns TRUE when all files are found", {
  mock_client <- list(
    get__files = function(prefix) {
      data.frame(name = prefix, stringsAsFactors = FALSE)
    }
  )

  # 2. Mock the client creator to return our mock_client
  local_mocked_bindings(
    create_client = function(...) mock_client,
    extract_api = function() list(host = "https://example.com", parameters = list()),
    .package = "Rega"
  )

  file_list <- list("file1.txt", "file2.csv")
  result <- files_in_inbox(file_list)

  expect_true(result)
  expect_type(result, "logical")
})

test_that("files_in_inbox returns FALSE if some files are missing or duplicated", {
  mock_client <- list(
    get__files = function(prefix) {
      if (prefix == "missing") {
        return(data.frame())
      }
      if (prefix == "duplicate") {
        return(data.frame(n = 1:2))
      }
      return(data.frame(n = 1))
    }
  )

  expect_false(files_in_inbox(list("file1", "missing"), client = mock_client))
  expect_false(files_in_inbox(list("file1", "duplicate"), client = mock_client))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("files_in_inbox throws error on empty file_list", {
  mock_client <- list(get__files = function(x) data.frame())

  expect_error(
    files_in_inbox(list(), client = mock_client),
    "must contain at least one element"
  )
})
