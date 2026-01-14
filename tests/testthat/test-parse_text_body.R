# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("parse_text_body handles single value", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('Simple text')
  )

  result <- parse_text_body(resp)

  expect_type(result, "character")
  expect_null(names(result))
  expect_equal(result[[1]], "Simple text")
})

test_that("parse_text_body handles complicated strings 1", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('user, "password"')
  )

  result <- parse_text_body(resp)

  expect_null(names(result))
  expect_equal(result[[1]], "user, \"password\"")
})

test_that("parse_text_body handles complicated strings 2", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('"{ invalid: json }"')
  )

  result <- parse_text_body(resp)

  expect_null(names(result))
  expect_equal(result[[1]], "\"{ invalid: json }\"")
})

test_that("parse_text_body handles json-like strings 1", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('["user", "password"]')
  )

  result <- parse_text_body(resp)

  expect_type(result, "list")
  expect_null(names(result))
  expect_equal(result, list("user", "password"))
})

test_that("parse_text_body handles json-like strings 2", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('{"foo": "user", "bar": "password"}')
  )

  result <- parse_text_body(resp)

  expect_type(result, "list")
  expect_named(result)
  expect_equal(result, list(foo = "user", bar =  "password"))
})

test_that("parse_text_body handles json-like strings 3", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('[{"foo": "user", "bar": "password"}]')
  )

  result <- parse_text_body(resp)

  expect_type(result, "list")
  expect_null(names(result))
  expect_equal(result, list(list(foo = "user", bar =  "password")))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("parse_text_body handles error paths and malformed JSON-like text", {
  resp <- httr2::response(
    headers = list("content-type" = "text/plain"),
    body = charToRaw('')
  )

  expect_error(parse_text_body(resp), "Can't retrieve empty body")

  expect_error(parse_text_body(list()), "must be an HTTP response object")
  expect_error(parse_text_body(NULL), "must be an HTTP response object")
  expect_error(parse_text_body(c()), "must be an HTTP response object")
  expect_error(parse_text_body(12345), "must be an HTTP response object")
  expect_error(parse_text_body("foobar"), "must be an HTTP response object")
})
