# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Some items in r, some not", {
  p <- c("a", "b", "c", "d")
  r <- c("b", "c")
  result <- add_required_str(p, r, req_str = "* ")

  expect_equal(result, c("a", "* b", "* c", "d"))
})

test_that("All items in p are in r", {
  p <- c("x", "y", "z")
  r <- c("x", "y", "z")
  result <- add_required_str(p, r)

  expect_equal(result, c("* x", "* y", "* z"))
})

test_that("None of the items in p are in r", {
  p <- c("m", "n", "o")
  r <- c("x", "y")
  result <- add_required_str(p, r, req_str = "* ")

  expect_equal(result, c("m", "n", "o"))
})

test_that("'r' is empty", {
  p <- c("m", "n", "o")
  r <- c()
  result <- add_required_str(p, r, req_str = "* ")

  expect_equal(result, c("m", "n", "o"))
})

test_that("'r' is not a vector or missing", {
  p <- c("a", "b")
  r <- data.frame(a = c("a", "b"))

  expect_equal(
    add_required_str(p, r),
    p
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'p' is not a character vector (e.g., numeric)", {
  p <- c(1, 2, 3)
  r <- c("1", "2")
  expect_error(
    add_required_str(p, r),
    "result is type 'double'"
  )
})

test_that("'req_str' is invalid (e.g., numeric or multiple values)", {
  p <- c("a", "b")
  r <- "a"
  invalid_req_str <- c("* ", "??")

  expect_error(
    add_required_str(p, r, req_str = invalid_req_str),
    "'req_str' must be a scalar character"
  )
})
