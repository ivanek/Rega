# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Simple schema without oneOf; payload is a valid JSON string", {
  schema <- list(
    type = "object",
    properties = list(
      name = list(type = "string")
    ),
    required = "name"
  )

  # Payload is already JSON
  payload <- '{"name": "Alice"}'

  result <- validate_schema(payload, schema)
  expect_true(result)
})

test_that("Simple schema, payload is an unboxed R list", {
  schema <- list(
    type = "object",
    properties = list(
      age = list(type = "number")
    ),
    required = "age"
  )

  payload <- list(age = 30)

  result <- validate_schema(unbox_list(payload), schema)
  expect_true(result)
})

test_that("Schema is empty", {
  payload <- '{"name": "Alice"}'
  # produces TRUE
  expect_true(validate_schema('{"name": "Alice"}', list()))
})

test_that("Simple schema, validation fails", {
  schema <- list(
    type = "object",
    properties = list(
      name = list(type = "string")
    ),
    required = "name"
  )

  payload <- data.frame(
    age = 30,
    name = 50
  )

  result <- validate_schema(unbox_row(payload), schema)
  errors <- attributes(result)$errors

  expect_false(result)
  expect_equal(dim(errors), c(1, 2))
  expect_equal(errors$field, "data.name")
  expect_equal(errors$message, "is the wrong type")
})

test_that("oneOf schema, payload matches one of the sub-schemas", {
  schema <- list(
    oneOf = list(
      list(
        type = "object",
        properties = list(name = list(type = "string")),
        required = "name"
      ),
      list(
        type = "object",
        properties = list(id = list(type = "integer")),
        required = "id"
      )
    )
  )

  # This payload only matches the first sub-schema (has "name", string)
  payload <- list(name = "TestName")

  result <- validate_schema(unbox_list(payload), schema)
  expect_true(result)
})

test_that("oneOf schema, payload doesn't match the sub-schemas", {
  schema <- list(
    oneOf = list(
      list(
        type = "object",
        properties = list(name = list(type = "string")),
        required = "name"
      ),
      list(
        type = "object",
        properties = list(id = list(type = "integer")),
        required = "id"
      )
    )
  )

  payload <- list(value = 30)
  result <- validate_schema(unbox_list(payload), schema)
  errors <- attributes(result)$errors

  expect_false(result)
  expect_equal(dim(errors), c(3, 2))
  expect_equal(errors$field, c("data", "data.name", "data.id"))
  expect_equal(
    errors$message,
    c("no (or more than one) schemas match", "is required", "is required")
  )
})

test_that("oneOf schema, payload doesn't match the sub-schemas", {
  schema <- list(
    oneOf = list(
      list(
        type = "object",
        properties = list(name = list(type = "string")),
        required = "name"
      ),
      list(
        type = "object",
        properties = list(id = list(type = "integer")),
        required = "id"
      )
    )
  )

  payload <- list(value = 30)
  result <- validate_schema(unbox_list(payload), schema)
  errors <- attributes(result)$errors

  expect_false(result)
  expect_equal(dim(errors), c(3, 2))
  expect_equal(errors$field, c("data", "data.name", "data.id"))
  expect_equal(
    errors$message,
    c("no (or more than one) schemas match", "is required", "is required")
  )
})


# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("invalid 'schema' object", {
  schema <- function(x) x

  payload <- '{"some": "data"}'
  expect_error(
    validate_schema(payload, schema),
    "'schema' must be a list"
  )
})

test_that("'payload' can't be converted to valid JSON", {
  con <- file()
  on.exit(close(con))

  schema <- list(type = "object")

  expect_warning(
    expect_error(validate_schema(con, schema), "No method asJSON"),
    "Payload was probably not processed"
  )
})

test_that("'payload' is a non-JSON string", {
  payload <- "Alice is a name"
  expect_error(validate_schema(payload, list()))
})

test_that("Payload list not processed by 'unbox_*' function", {
  schema <- list(
    type = "object",
    properties = list(
      age = list(type = "number")
    ),
    required = "age"
  )

  payload <- list(age = 30)

  expect_warning(
    expect_false(validate_schema(payload, schema)),
    "Payload was probably not processed"
  )
})

test_that("Payload data frame not processed by 'unbox_*' function", {
  schema <- list(
    type = "object",
    properties = list(
      age = list(type = "number")
    ),
    required = "age"
  )

  payload <- data.frame(age = 30)

  expect_warning(
    expect_false(validate_schema(payload, schema)),
    "Payload was probably not processed"
  )
})
