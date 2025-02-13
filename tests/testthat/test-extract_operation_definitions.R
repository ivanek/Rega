# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single path with a single method (operation_id present)", {
  api <- list(
    paths = list(
      "/items" = list(
        get = list(
          operation_id = "getItems",
          parameters = list("paramA", "paramB")
        )
      )
    )
  )

  ops <- extract_operation_definitions(api)
  expect_length(ops, 1)
  expect_named(ops, "getItems")
  expect_equal(ops[["getItems"]]$method, "GET")
  expect_equal(ops[["getItems"]]$path, "/items")
  expect_equal(ops[["getItems"]]$parameters, list("paramA", "paramB"))
})

test_that("Multiple paths, some with multiple methods", {
  api <- list(
    paths = list(
      "/users" = list(
        get = list(operation_id = "getUsers"),
        post = list(operation_id = "createUser")
      ),
      "/users/{id}" = list(
        patch = list(operation_id = "updateUser")
      )
    )
  )

  ops <- extract_operation_definitions(api)
  expect_equal(length(ops), 3)
  expect_named(ops, c("getUsers", "createUser", "updateUser"))
  expect_equal(ops[["updateUser"]]$method, "PATCH")
  expect_equal(ops[["updateUser"]]$path, "/users/{id}")
})

test_that("Missing operation_id", {
  api <- list(
    paths = list(
      "/items/{item_id}" = list(
        get = list(
          parameters = list("paramX"),
          requestBody = list(
            required = TRUE,
            content = list(
              `application/json` = list(
                schema = list(properties = "property")
              )
            )
          )
        )
      )
    )
  )

  ops <- extract_operation_definitions(api)

  expect_length(ops, 1)
  generated_id <- names(ops)[1]
  expect_true(grepl("^get__items__item_id$", generated_id))
  expect_equal(ops[[generated_id]]$method, "GET")
  expect_equal(ops[[generated_id]]$path, "/items/{item_id}")
  expect_equal(ops[[generated_id]]$parameters, list("paramX"))
  expect_true(ops[[generated_id]]$requestBody$required)
})

test_that("Complex example api", {
  api <- extract_api("../resources/petstore_resolved_openapi_v3_1.yaml")
  ops <- extract_operation_definitions(api)

  expect_length(ops, 19)
  expect_true(all(c(
    "get__pet_findByStatus", "get__pet_findByTags", "get__store_order__orderId",
    "post__user_createWithList"
  ) %in% names(ops)))
  opid1 <- names(ops)[1]
  expect_equal("put__pet", opid1)
  expect_equal(ops[[opid1]]$method, "PUT")
  expect_equal(
    ops[[opid1]]$security[[1]]$petstore_auth,
    c("write:pets", "read:pets")
  )
  expect_equal(length(ops[[opid1]]$requestBody), 3)
})

test_that("'api$paths' is missing or not a list", {
  api <- list()
  expect_equal(extract_operation_definitions(api), list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'api' is not a list => subscript fails", {
  api <- "invalid"
  expect_error(
    extract_operation_definitions(api),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})

test_that("Operation definition is not a list", {
  api <- list(
    paths = list(
      "/items" = list(
        get = "some string, not a list"
      )
    )
  )

  expect_error(
    extract_operation_definitions(api),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})
