# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("parse_json_body handles happy paths and standardization", {
  resp1 <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw(
      '[{"id": 12345, "title": "submission", "description": "this submission"},
        {"id": 56789, "title": "studies", "description": "this study"} ]'
    )
  )

  result1 <- parse_json_body(resp1)

  expect_type(result1, "list")
  expect_equal(length(result1), 2)
  expect_equal(length(result1[[1]]), 3)
  expect_equal(result1[[1]]$id, 12345)
  expect_null(names(result1))
  expect_named(result1[[1]])
})

test_that("parse_json_body handles json array without names", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw(
      '[[12345, "submission","this submission"],
        [56789 ,"studies", "this study"]]'
    )
  )

  result <- parse_json_body(resp)

  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(length(result[[1]]), 3)
  expect_equal(result[[1]][[1]], 12345)
  expect_null(names(result))
  expect_null(names(result[[1]]))
})

test_that("parse_json_body handles single array", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw('["user", "password"]')
  )

  expect_equal(parse_json_body(resp), list("user", "password"))
})

test_that("parse_json_body handles single dict", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw('{"name": "Bob"}')
  )

  expect_equal(parse_json_body(resp), list(name = "Bob"))
})

test_that("parse_json_body handles single value", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw('"user"')
  )

  expect_equal(parse_json_body(resp), list("user"))
})

test_that("parse_json_body handles empty json array", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw("[]")
  )

  expect_equal(parse_json_body(resp), list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("parse_json_body handles error paths and unexpected structures", {
  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw("")
  )

  expect_error(parse_json_body(resp), "Can't retrieve empty body")

  resp <- httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw('"user, password')
  )

  expect_error(parse_json_body(resp), "premature EOF")

  expect_error(parse_json_body(list()), "must be an HTTP response object")
  expect_error(parse_json_body(NULL), "must be an HTTP response object")
  expect_error(parse_json_body(c()), "must be an HTTP response object")
  expect_error(parse_json_body(12345), "must be an HTTP response object")
  expect_error(parse_json_body("foobar"), "must be an HTTP response object")
})
