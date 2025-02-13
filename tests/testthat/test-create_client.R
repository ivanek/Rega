# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("API with one operation definition", {
  api <- list(paths = list(pet = list(
    post = list(
      tags = "pet", summary = "Add a new pet to the store",
      description = "Add a new pet to the store", operationId = "addPet",
      security = list(
        list(petstore_auth = c("write:pets", "read:pets"))
      ),
      requestBody = list(
        description = "Create a new pet in the store",
        required = TRUE, content = list(
          `application/json` = list(
            schema = list(`$ref` = "#/components/schemas/Pet")
          )
        )
      )
    )
  )))

  client <- create_client(api)

  expect_length(client, 1)
  expect_named(client, "post_pet")
  expect_true(is.function(client$post_pet))
})

test_that("Test petstore API with multiple operation definitions", {
  api <- extract_api("../resources/petstore_resolved_openapi_v3_1.yaml")

  client <- create_client(api)

  expect_length(client, 19)
  expect_named(client)
  expect_true(all(
    c(
      "get__pet_findByStatus", "get__pet_findByTags",
      "get__pet__petId", "delete__store_order__orderId"
    ) %in% names(client)
  ))
  expect_true(all(vapply(client, is.function, logical(1))))
})

test_that("No operation definitions", {
  api <- list(opdefs = list())
  client <- create_client(api)
  expect_equal(client, list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'api' is not a list", {
  api <- "invalid"
  expect_error(
    create_client(api),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})

test_that("Internal factory fails", {
  local_mocked_bindings(
    api_function_factory = function(op_def, api, ...) stop("Simulated factory error"),
    .package = "Rega"
  )

  api <- extract_api("../resources/petstore_resolved_openapi_v3_1.yaml")

  expect_error(
    create_client(api),
    "Simulated factory error"
  )
})
