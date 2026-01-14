# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Text/plain content", {
  resp <- httr2::response(
    url = "https://api.example.com/api/abc",
    headers = list("content-type" = "text/plain"),
    body = charToRaw("This is a plain-text response.")
  )

  result <- parse_ega_body(resp)
  expect_s3_class(result, "tbl_df")
  expect_equal(dim(result), c(1, 1))
  expect_true(grepl("This is a plain-text response", result$abc))
})

test_that("Text/plain that looks like a single entry json", {
  resp <- httr2::response(
    url = "https://api.example.com/api/user",
    headers = list("content-type" = "application/json"),
    body = charToRaw('{"user":"alice","age":30}')
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(dim(result), c(1, 2))
  expect_equal(result$user, c("alice"))
  expect_equal(result$age, c(30))
})

test_that("Text/plain content that looks like a single entry json inside array", {
  resp <- httr2::response(
    url = "https://api.example.com/api/user",
    headers = list("content-type" = "text/plain"),
    body = charToRaw('[{"user":"alice","age":30}]')
  )

  result <- parse_ega_body(resp)

  expect_equal(dim(result), c(1, 2))
  expect_equal(result$user, c("alice"))
  expect_equal(result$age, c(30))
})

test_that("Application/json content => multi row data frame", {
  resp <- httr2::response(
    url = "https://api.example.com/api/user",
    headers = list("content-type" = "application/json"),
    body = charToRaw('[{"user":"alice","age":30},{"user":"bob","age":20}]'),
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("user", "age"))
  expect_equal(result$user, c("alice", "bob"))
  expect_equal(result$age, c(30, 20))
})

test_that("Application/json content => simple entry", {
  resp <- httr2::response(
    url = "https://api.example.com/api/name",
    headers = list("content-type" = "application/json"),
    body = charToRaw('"alice"'),
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("name"))
  expect_equal(result$name, c("alice"))
})

test_that("Application/json content => single column", {
  # gets column name from resource
  resp <- httr2::response(
    url = "https://api.example.com/api/name",
    headers = list("content-type" = "application/json"),
    body = charToRaw('[{"user":"alice"},{"user":"bob"}]'),
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("name"))
  expect_equal(result$name, c("alice", "bob"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Is not httr2_response object", {
  resp <- list(charToRaw("This is a plain-text response."))
  expect_error(parse_ega_body(resp), "must be an 'httr2_response' object")
  expect_error(parse_ega_body(data.frame()), "must be an 'httr2_response' object")
  expect_error(parse_ega_body(c("foo", "bar")), "must be an 'httr2_response' object")
})

test_that("Unknown resp_content_type", {
  resp <- httr2::response(
    url = "https://api.example.com/api/images",
    headers = list("content-type" = "image/png"),
    body = charToRaw("This is a plain-text response.")
  )

  expect_error(
    parse_ega_body(resp),
    "Unknown content type"
  )
})

test_that("JSON parse fails => fromJSON throws error", {
  resp <- httr2::response(
    url = "https://api.example.com/api/name",
    headers = list("content-type" = "application/json"),
    body = charToRaw('[{"user":"alice"},{"user":"bob"') # error
  )

  expect_error(
    parse_ega_body(resp),
    "parse error"
  )
})

test_that("Text/plain JSON parse fails => fromJSON throws error", {
  resp <- httr2::response(
    url = "https://api.example.com/api/name",
    headers = list("content-type" = "application/json"),
    body = charToRaw('{"user":"alice","age"30}') # error
  )

  expect_error(
    parse_ega_body(resp),
    "parse error"
  )
})

test_that("Text/plain JSON like missing brace", {
  resp <- httr2::response(
    url = "https://api.example.com/api/name",
    headers = list("content-type" = "text/plain"),
    body = charToRaw('{"user":"alice","age:"30') # missing brace regex FALSE
  )

  result <- parse_ega_body(resp)

  expect_equal(result$name, "{\"user\":\"alice\",\"age:\"30")
})

test_that("URL path extraction fails, unexpected tibble column name", {
  resp <- httr2::response(
    url = "some/unexpected/format",
    headers = list("content-type" = "text/plain"),
    body = charToRaw("This is a plain-text response.")
  )

  expect_error(
    parse_ega_body(resp),
    "Failed to parse URL"
  )
})

test_that("api/ string not in URL path", {
  resp <- httr2::response(
    url = "https://api.example.com/otherpath/abc",
    headers = list("content-type" = "text/plain"),
    body = charToRaw("This is a plain-text response.")
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_false(names(result) == c("abc"))
})
