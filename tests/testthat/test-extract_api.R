example_api <- "../resources/petstore_resolved_openapi_v3_1.yaml"

# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Extract default EGA API", {
  result <- extract_api()

  expect_type(result, "list")
  expect_length(result, 10)
  expect_equal(
    names(result),
    c(
      "openapi", "info", "servers", "externalDocs", "security", "tags", "paths",
      "components", "host", "basePath"
    )
  )
  expect_length(result$info, 5)
  expect_length(result$paths, 79)
  expect_equal(result$host, "https://submission.ega-archive.org/api")
})

test_that("Extract example API", {
  result <- extract_api(example_api)

  expect_type(result, "list")
  expect_length(result, 9)
  expect_length(result$paths, 13)
  expect_named(
    result,
    c(
      "openapi", "info", "externalDocs", "servers", "tags", "paths",
      "components", "host", "basePath"
    )
  )
  expect_equal(length(result$paths), 13)
  expect_named(
    result$paths,
    c("/pet", "/pet/findByStatus", "/pet/findByTags", "/pet/{petId}",
      "/pet/{petId}/uploadImage", "/store/inventory", "/store/order",
      "/store/order/{orderId}", "/user", "/user/createWithList", "/user/login",
      "/user/logout", "/user/{username}")
  )
  expect_false(grepl("/$",result$basePath))
})

test_that("Extract example API with custom host", {
  result <- extract_api(example_api, "www.example.com")

  expect_type(result, "list")
  expect_length(result, 9)
  expect_named(
    result,
    c(
      "openapi", "info", "externalDocs", "servers", "tags", "paths",
      "components", "host", "basePath"
    )
  )
  expect_equal(result$host, "www.example.com")
  expect_false(grepl("/$",result$basePath))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Spec file is not yaml/json", {
  expect_error(
    extract_api("example.txt"),
    "Specification file does not appear to be JSON or YAML"
  )
})

test_that("No path elements in the API", {
  api <-"../resources/malformed_api_1.yaml"
  expect_warning(
    extract_api(api),
    "There is no paths element in the API specification"
  )
})

test_that("No servers url entry in yaml and no host specified as argument", {
  api <-"../resources/malformed_api_2.yaml"
  expect_error(
    extract_api(api),
    "Host URL not supplied and not found in specification file"
  )
})

