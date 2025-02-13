# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("A non-empty list with single element of length 1", {
  my_list <- list(a = 10)
  result <- unbox_list(my_list)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(1,1))
  expect_equal(result$a, 10)
})

test_that("multiple single-length elements => returns data frame with matching columns", {
  my_list <- list(x = 1L, y = "test", z = factor("A"))
  result <- unbox_list(my_list)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(1,3))
  expect_equal(result$x, 1L)
  expect_equal(result$y, "test")
  expect_equal(result$z, "A")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("An empty list", {
  expect_error(
    unbox_list(list()),
    "Only atomic vectors of length 1.*can be unboxed"
  )
})

test_that("One element has length 2", {
  my_list <- list(a = 10, b = c("x", "y"))
  expect_error(
    unbox_list(my_list),
    "All elements of the list must be of length 1"
  )
})

test_that("One element has length 0 => triggers error message", {
  my_list <- list(a = 10, b = character(0))
  expect_error(
    unbox_list(my_list),
    "All elements of the list must be of length 1\\."
  )
})

test_that("A vector type passed => creates multi-row data frame", {
  my_vec <- c("not", "a", "list")
  expect_error(
    unbox_list(my_vec),
    "Tried to unbox dataframe with 3 rows"
  )
})

test_that("A list containing a function => length(function) is not 1 => error", {
  my_list <- list(a = 10, b = function(x) x + 1)
  expect_error(
    unbox_list(my_list),
    "cannot coerce class.*to a data.frame"
  )
})
