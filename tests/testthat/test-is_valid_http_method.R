# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("A standard uppercase method", {
  expect_true(is_valid_http_method("GET"))
})

test_that("A lowercase method", {
  expect_true(is_valid_http_method("post"))
})

test_that("Mixed-case method", {
  expect_true(is_valid_http_method("pAtCh"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("NULL method", {
  expect_false(is_valid_http_method(NULL))
})

test_that("Empty string", {
  expect_false(is_valid_http_method(""))
})

test_that("Random unsupported method", {
  expect_false(is_valid_http_method("FOO"))
})

test_that("Non-character method", {
  expect_false(is_valid_http_method(123))
})

test_that("vector or list input", {
  expect_error(is_valid_http_method(c(1,2,3)), "the condition has length > 1")
  expect_error(is_valid_http_method(list(1,2,3)), "the condition has length > 1")
})
