# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("is_provisional handles numeric inputs correctly", {
  expect_true(is_provisional(100))
  expect_true(is_provisional(1.0))
  expect_false(is_provisional(100.5))
  expect_true(is_provisional(1e6))
  expect_true(is_provisional(0))
  expect_type(is_provisional(100), "logical")
})

test_that("is_provisional handles character inputs correctly", {
  expect_true(is_provisional("5"))
  expect_true(is_provisional("99"))
  expect_true(is_provisional("12345"))

  expect_false(is_provisional("0123")) # Leading zero
  expect_false(is_provisional("A123")) # Alpha
  expect_type(is_provisional("123"), "logical")
})

test_that("is_provisional handles vector inputs", {
  expect_equal(is_provisional(c(1, 1.5, 2)), c(TRUE, FALSE, TRUE))
  expect_equal(is_provisional(c("12", "01", "abc")), c(TRUE, FALSE, FALSE))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("is_provisional throws errors for invalid types", {
  expect_error(is_provisional(TRUE), "Unknown type of provisional ID")
  expect_error(is_provisional(list(123)), "Unknown type of provisional ID")
  expect_error(
    is_provisional(as.factor("123")),
    "Unknown type of provisional ID"
  )
})
