# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("No existing records => calls submit_table => returns posted data", {
  mock_client <- list(
    `get__submissions__provisional_id__endpointA` = function(id) NULL,
    `post__submissions__provisional_id__endpointA` = function(id, body) {
      # Pretend we "post" and return a data frame with the same rows
      return(as.data.frame(body))
    }
  )

  my_data <- data.frame(x = 1:2, y = c("A", "B"))
  result <- get_or_post(
    submission_id = "id0001",
    data = my_data,
    client = mock_client,
    endpoint = "endpointA",
    retrieve_if_exists = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(my_data))
  expect_equal(result, my_data)
})

test_that("Some existing records => same number => retrieve_if_exists=TRUE => returns existing", {
  my_data <- data.frame(x = 1:2, y = c("A", "B"))
  mock_existing <- my_data

  mock_client <- list(
    `get__submissions__provisional_id__endpointA` = function(sub_id) {
      mock_existing
    },
    `post__submissions__provisional_id__endpointA` = function(sub_id, body) {
      stop("Should not be called in this scenario.")
    }
  )

  expect_message(
    result <- get_or_post(
      submission_id = "id0002",
      data = my_data,
      client = mock_client,
      endpoint = "endpointA",
      retrieve_if_exists = TRUE
    ),
    "Retrieved IDs from database."
  )

  expect_equal(result, mock_existing)
})

test_that("Get returns a 0-row data frame => calls submit_table => success", {
  mock_client <- list(
    `get__submissions__provisional_id__endpointB` = function(sub_id) {
      data.frame(x = numeric(0), y = character(0))
    },
    `post__submissions__provisional_id__endpointB` = function(sub_id, body) {
      return(as.data.frame(body))
    }
  )

  my_data <- data.frame(x = 1, y = "OnlyOneRow")
  result <- get_or_post(
    submission_id = "id003",
    data = my_data,
    client = mock_client,
    endpoint = "endpointB",
    retrieve_if_exists = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result, my_data)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Mismatch in row count => stops with error", {
  # The GET returns 2 rows, but 'data' has 3 rows => mismatch => error
  mock_client <- list(
    `get__submissions__provisional_id__endpointC` = function(sub_id) {
      data.frame(a = 1:2)
    },
    `post__submissions__provisional_id__endpointC` = function(...) stop("Should not post")
  )

  my_data <- data.frame(a = 1:3)

  expect_error(
    get_or_post(
      submission_id = "SUB_MISMATCH",
      data = my_data,
      client = mock_client,
      endpoint = "endpointC",
      retrieve_if_exists = FALSE
    ),
    "Number of present versus submitted records doesn't match"
  )
})

test_that("Same number of existing rows but retrieve_if_exists=FALSE => error", {
  mock_client <- list(
    `get__submissions__provisional_id__endpointZ` = function(sub_id) {
      data.frame(b = c("A", "B"))
    },
    `post__submissions__provisional_id__endpointZ` = function(...) stop("Should not post")
  )

  my_data <- data.frame(b = c("X", "Y"))
  expect_error(
    get_or_post(
      submission_id = "SUB_SAME_ROWS",
      data = my_data,
      client = mock_client,
      endpoint = "endpointZ",
      retrieve_if_exists = FALSE
    ),
    "records are already present"
  )
})

test_that("Client is not a function", {
  mock_client <- list()

  expect_error(
    get_or_post(
      data = data.frame(),
      client = mock_client,
      endpoint = "endpointA"
    ),
    "attempt to apply non-function"
  )
})

test_that("'client' does not have the needed get/post method => error", {
  # If 'client[[paste0("get", built_url)]]' is missing,
  # we get a subscript out-of-bounds or NULL call error
  mock_client <- list(
    `post__submissions__provisional_id__endpointA` = function(...) data.frame()
  )

  expect_error(
    get_or_post(
      submission_id = "MISSING_GET",
      data = data.frame(x = 1),
      client = mock_client,
      endpoint = "endpointA"
    ),
    "subscript out of bounds|attempt to apply non-function"
  )
})

test_that("Submit_table fails (simulated by throwing an error in post method)", {
  mock_client <- list(
    `get__submissions__provisional_id__endpointX` = function(sub_id) NULL,
    `post__submissions__provisional_id__endpointX` = function(sub_id, body) {
      stop("Simulated post error")
    }
  )

  my_data <- data.frame(z = 1)

  # We expect the error from the post method to bubble up
  expect_error(
    get_or_post(
      submission_id = "SHOULD_FAIL",
      data = my_data,
      client = mock_client,
      endpoint = "endpointX"
    ),
    "Simulated post error"
  )
})
