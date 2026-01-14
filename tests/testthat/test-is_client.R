# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".is_client identifies valid client objects", {
  client_1 <- list(
    GET_users = function() "result",
    POST_data = function(x) x
  )

  result <- .is_client(client_1)
  expect_true(result)
  expect_type(result, "logical")

  client_2 <- list(
    PATCH_v1_update_user = function() NULL
  )
  expect_true(.is_client(client_2))

  client_3 <- list(
    DELETE_all = function() NULL,
    HEAD_check = function() NULL
  )
  expect_true(.is_client(client_3))

  # Gets converted to list automatically
  client_4 <- c(GET_test = function() NULL)
  expect_true(.is_client(client_4))

  no_underscore_client <- list(
    GET = function() NULL
  )
  expect_true(.is_client(no_underscore_client))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".is_client catches structural and type errors", {
  invalid_content <- list(
    GET_user = function() NULL,
    POST_data = "I am a string, not a function"
  )
  expect_error(.is_client(invalid_content), "'client' must be a list of functions.")

  bad_method_client <- list(
    FETCH_data = function() NULL
  )
  expect_error(
    .is_client(bad_method_client),
    "'client' function names must start with valid http method name."
  )

  wrong_name_client <- list(
    myfunction = function() NULL
  )
  expect_error(.is_client(wrong_name_client))

  empty_list_client <- list()
  expect_error(.is_client(empty_list_client), "must be a non-empty list")
})
