# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Text/plain content", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/abc",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw("This is a plain-text response.")
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)
  expect_s3_class(result, "tbl_df")
  expect_equal(dim(result), c(1,1))
  expect_true(grepl("This is a plain-text response", result$abc))
})

test_that("Text/plain that looks like a single entry json", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/user",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('{"user":"alice","age":30}'),
      cache = new.env()
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)
  expect_s3_class(result, "tbl_df")
  expect_equal(dim(result), c(1, 2))
  expect_equal(result$user, c("alice"))
  expect_equal(result$age, c(30))
})


test_that("Application/json content => multi row data frame", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "application/json; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('[{"user":"alice","age":30},{"user":"bob","age":20}]'),
      cache = new.env()
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("user", "age"))
  expect_equal(result$user, c("alice", "bob"))
  expect_equal(result$age, c(30, 20))
})

test_that("Application/json content => simple entry", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "application/json; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('"alice"'),
      cache = new.env()
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("name"))
  expect_equal(result$name, c("alice"))
})

test_that("Application/json content => single column", {
  # gets column name from resource
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "application/json; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('[{"user":"alice"},{"user":"bob"}]'),
      cache = new.env()
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("name"))
  expect_equal(result$name, c("alice", "bob"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Unknown resp_content_type", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/images",
      status_code = 200L,
      headers = list(
        "content-type" = "image/png",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw("This is a plain-text response.")
    ),
    class = "httr2_response"
  )

  expect_error(
    parse_ega_body(resp),
    "Unknown content type"
  )
})

test_that("JSON parse fails => fromJSON throws error", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "application/json; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('[{"user":"alice"},{"user":"bob"'), # error
      cache = new.env()
    ),
    class = "httr2_response"
  )

  expect_error(
    parse_ega_body(resp),
    "parse error"
  )
})

test_that("Text/plain JSON parse fails => fromJSON throws error", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('{"user":"alice","age"30}'), # error
      cache = new.env()
    ),
    class = "httr2_response"
  )

  expect_error(
    parse_ega_body(resp),
    "parse error"
  )
})

test_that("Text/plain JSON like missing brace", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/api/name",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw('{"user":"alice","age:"30'), # missing brace regex FALSE
      cache = new.env()
    ),
    class = "httr2_response"
  )

  result = parse_ega_body(resp)

  expect_equal(result$name, "{\"user\":\"alice\",\"age:\"30")
})

test_that("URL path extraction fails, unexpected tibble column name", {
  resp <- structure(
    list(
      method = "GET",
      url = "some/unexpected/format",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw("This is a plain-text response.")
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_false(names(result) == c("format"))
  expect_true(names(result) == c("some/unexpected/format"))
})

test_that("api/ string not in URL path", {
  resp <- structure(
    list(
      method = "GET",
      url = "https://api.example.com/otherpath/abc",
      status_code = 200L,
      headers = list(
        "content-type" = "text/plain; charset=utf-8",
        "date" = "Mon, 20 Feb 2023 10:00:00 GMT"
      ),
      body = charToRaw("This is a plain-text response.")
    ),
    class = "httr2_response"
  )

  result <- parse_ega_body(resp)

  expect_s3_class(result, "tbl_df")
  expect_false(names(result) == c("abc"))
})
