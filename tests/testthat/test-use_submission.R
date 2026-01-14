# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("successfully performs 'get' operation for all endpoints using an accession ID", {
  all_ops <- c("datasets", "analyses", "runs", "experiments", "samples", "studies")

  mock_client <- lapply(all_ops, function(op) {
    function(id) paste("got", op, "for", id)
  })
  names(mock_client) <- paste0("get__submissions__accession_id__", all_ops)

  result <- use_submission(id = "EGAB12345678901", method = "GET", client = mock_client)

  expect_type(result, "list")
  expect_length(result, 6)
  expect_named(result, all_ops)
  expect_equal(result$datasets, "got datasets for EGAB12345678901")
})

test_that("successfully performs 'delete' operation using a provisional ID", {
  mock_client <- list(
    delete__submissions__provisional_id__samples = function(id) "deleted samples"
  )

  result <- use_submission(id = 12345, method = "delete", client = mock_client)

  expect_type(result, "list")
  expect_equal(result$samples, "deleted samples")
  expect_null(result$datasets)
  expect_null(result$studies)
})

test_that("handles mixed case method names", {
  mock_client <- list(get__submissions__provisional_id__studies = function(id) "ok")

  result <- use_submission(id = "123", method = "GeT", client = mock_client)
  expect_equal(result$studies, "ok")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("throws error if method is not a character scalar", {
  expect_error(
    use_submission(id = "EGAB12345678901", method = 123),
    "must be a non-empty character scalar"
  )
  expect_error(
    use_submission(id = "EGAB12345678901", method = list()),
    "must be a non-empty character scalar"
  )
})

test_that("throws error when ID matches neither accession nor provisional types", {
  expect_error(
    use_submission(id = "invalid_id", method = "get"),
    "Unknown ID type"
  )
  expect_error(
    use_submission(id = NULL, method = "get"),
    "Unknown ID type"
  )
  expect_error(
    use_submission(id = list(), method = "get"),
    "Unknown ID type"
  )
})

test_that("throws error for unsupported methods", {
  expect_error(
    use_submission(id = "EGAB12345678901", method = "post"),
    "Only 'get' and 'delete' methods are currently supported"
  )
  expect_error(
    use_submission(id = "EGAB12345678901", method = "put"),
    "Only 'get' and 'delete' methods are currently supported"
  )
})

test_that("throws error if client is incorrect ", {
  expect_error(
    use_submission(id = "EGAB12345678901", method = 123),
    "must be a non-empty character scalar"
  )
  expect_error(
    use_submission(id = "EGAB12345678901", method = list()),
    "must be a non-empty character scalar"
  )
})
