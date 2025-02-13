# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Minimal valid structure, returns the schema element", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list(
          schema = list(type = "object", properties = list(a = "string"))
        )
      )
    )
  )

  result <- get_operation_schema(op)

  expect_true(is.list(result))
  expect_equal(result$type, "object")
  expect_named(result$properties, "a")
})

test_that("Schema is just a plain list (no advanced properties)", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list(
          schema = list(description = "Just a plain schema")
        )
      )
    )
  )

  result <- get_operation_schema(op)

  expect_type(result, "list")
  expect_equal(result$description, "Just a plain schema")
})

test_that("Nested schema object with multiple keys", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list(
          schema = list(
            type = "array",
            items = list(type = "string"),
            additionalProperties = FALSE
          )
        )
      )
    )
  )

  result <- get_operation_schema(op)
  expect_equal(result$type, "array")
  expect_named(
    result, c("type", "items", "additionalProperties"),
    ignore.order = TRUE
  )
})

test_that("requestBody or content is missing => returns NULL", {
  expect_null(get_operation_schema(list()))
})

test_that("The schema element is missing => returns NULL", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list()
      )
    )
  )
  result <- get_operation_schema(op)
  expect_null(result)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'op' is not a list", {
  op <- "I am a string"

  expect_error(
    get_operation_schema(op),
    "\\$ operator is invalid|object of type 'character' is not subsettable"
  )
})
