# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("get_schemas extracts correct Request schemas", {
  mock_api <- list(
    components = list(
      schemas = list(
        UserRequest = list(type = "object"),
        AuthRequest = list(type = "string"),
        UserResponse = list(type = "object"), # Filtered out
        Metadata = NULL # Filtered out
      )
    )
  )

  result <- get_schemas(mock_api)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("UserRequest", "AuthRequest"))
  expect_equal(result$UserRequest$type, "object")
})

test_that("get_schemas handles boundary cases like empty or non-matching schemas", {
  mock_api_no_match <- list(
    components = list(
      schemas = list(UserResponse = list(), Status = list())
    )
  )

  result_empty <- get_schemas(mock_api_no_match)
  expect_length(result_empty, 0)
  expect_type(result_empty, "list")

  large_schemas <- setNames(
    replicate(100, list(type = "string"), simplify = FALSE),
    paste0("Schema", 1:100, "Request")
  )
  mock_api_large <- list(components = list(schemas = large_schemas))

  result_large <- get_schemas(mock_api_large)
  expect_length(result_large, 100)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("get_schemas catches structural errors in the api list", {
  expect_error(get_schemas(123), "api' argument must be a list")
  expect_error(get_schemas(list(a = 1)), "contain an element named 'components'")
  expect_error(get_schemas(list(components = "not_a_list")), "must be a list")
  expect_error(
    get_schemas(list(components = list(other = 1))),
    "contain an element named 'schemas'"
  )
})
