# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("correctly routes to provisional_id endpoint and returns a list", {
  mock_resp <- list(files = list("file1.txt"), metadata = list())

  mock_client <- list(
    get__submissions__provisional_id = function(id) list(id = 12345, type = "provisional")
  )

  local_mocked_bindings(
    create_client = function(api) mock_client,
    use_submission = function(id, client, action) mock_resp
  )

  result <- get_submission(id = 12345)

  expect_type(result, "list")
  expect_named(result, c("submission", "files", "metadata"))
  expect_equal(result$submission$type, "provisional")
  expect_equal(result$files, list("file1.txt"))
})

test_that("correctly routes to accession_id endpoint for EGA accessions", {
  mock_id <- "EGAB00000000001"
  mock_resp <- list(id = mock_id, type = "accession", extra = "data")

  mock_client <- list(
    get__submissions__accession_id = function(id) mock_resp
  )

  result <- get_submission(id = mock_id, client = mock_client)

  expect_equal(result$submission$id, mock_id)
  expect_equal(result$submission$type, "accession")
  expect_null(result$studies)
  expect_null(result$samples)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("throws error when ID matches neither accession nor provisional types", {
  expect_error(
    get_submission(id = "invalid_id", method = "get"),
    "Unknown ID type"
  )
  expect_error(
    get_submission(id = NULL, method = "get"),
    "Unknown ID type"
  )
  expect_error(
    get_submission(id = list(), method = "get"),
    "Unknown ID type"
  )
})

test_that("raises error when client validation fails", {
  expect_error(get_submission(id = "EGAB00000000001", client = list()), "must be a non-empty list")
  expect_error(get_submission(id = "EGAB00000000001", client = c("aaa")), "must be a non-empty list")
})

test_that("propagates errors from the dynamic client method call", {
  mock_client <- list(
    get__submissions__provisional_id = function(id) stop("404 Not Found")
  )

  expect_error(get_submission(id = 12345, client = mock_client), "404 Not Found")
})
