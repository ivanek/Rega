# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".is_scalar identifies valid scalar inputs (Happy Paths)", {
  # Standard numeric and character scalars
  expect_true(.is_scalar(10))
  expect_true(.is_scalar("hello"))

  # Logical and edge-case numerics
  expect_true(.is_scalar(TRUE))
  expect_true(.is_scalar(Inf))
  expect_true(.is_scalar(NA)) # NAs are atomic and length 1

  # Large values
  expect_true(.is_scalar(1e10))

  # Check output type is always logical
  expect_type(.is_scalar(1), "logical")
})

test_that(".is_scalar handles boundary cases for length and type", {
  expect_false(.is_scalar(numeric(0)))
  expect_false(.is_scalar(c(1, 2)))

  expect_false(.is_scalar(1 + 2i))
  expect_false(.is_scalar(as.complex(5)))
})

test_that(".is_scalar correctly rejects non-atomic or multi-dimensional structures", {
  expect_false(.is_scalar(list(1)))

  example_df <- data.frame(a = 1)
  expect_false(.is_scalar(example_df))

  expect_false(.is_scalar(NULL))

  expect_true(.is_scalar(matrix(1, 1, 1)))
})

test_that(".is_scalar returns FALSE for non-logical bounds like functions or environments", {
  expect_false(.is_scalar(mean))
  expect_false(.is_scalar(new.env()))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------
