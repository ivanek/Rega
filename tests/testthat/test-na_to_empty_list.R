# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Replaces scalar NA elements with empty list, leaves others intact", {
  input <- list(NA, "hello", 123, NA, TRUE)
  result <- na_to_empty_list(input)

  expect_length(result, 5)
  expect_identical(result[[1]], list())
  expect_identical(result[[2]], "hello")
  expect_identical(result[[3]], 123)
  expect_identical(result[[4]], list())
  expect_identical(result[[5]], TRUE)
})

test_that("Empty list remains empty list", {
  expect_equal(na_to_empty_list(list()), list())
})

test_that("'l' is a vector", {
  not_a_list <- c(NA, 1, 2)
  result <- na_to_empty_list(not_a_list)

  expect_length(result, 3)
  expect_equal(result, list(list(), 1, 2))
})

test_that("'l' is NULL", {
  expect_equal(na_to_empty_list(NULL), list())
})

test_that("Elements that are vectors with partial NAs", {
  input <- list(c(NA, 2), c(3, NA))
  expect_equal(na_to_empty_list(input), input)
})

test_that("Passing objects that result in logical(0) after is.na", {
  input <- list(new.env())
  expect_warning(
    expect_equal(na_to_empty_list(input), list(list())),
    "is.na\\(\\) applied to non-\\(list or vector\\)"
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("l is not list, vector or NULL", {
  expect_error(na_to_empty_list(new.env()), "must be a list, vector or NULL")
  expect_error(na_to_empty_list(function() {}), "must be a list, vector or NULL")
})
