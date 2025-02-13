# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single transformation", {
  df <- data.frame(
    fruit = c("Apple", "Banana"),
    stringsAsFactors = FALSE
  )
  fruit_lut <- list(Apple = "Red", Banana = "Yellow")
  result <- multi_lut_add(
    df,
    list("color", "fruit", fruit_lut)
  )

  expect_equal(result$color, c("Red", "Yellow"))
  expect_true("color" %in% names(result))
})

test_that("Multiple transformations", {
  df <- data.frame(
    fruit = c("Apple", "Banana"),
    size = c("Large", "Small"),
    stringsAsFactors = FALSE
  )

  fruit_lut <- list(Apple = "Red", Banana = "Yellow")
  size_lut <- list(Large = "Big", Small = "Tiny")

  result <- multi_lut_add(
    df,
    list("color", "fruit", fruit_lut),
    list("descriptor", "size", size_lut)
  )

  expect_equal(result$color, c("Red", "Yellow"))
  expect_equal(result$descriptor, c("Big", "Tiny"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("One of the lists has length != 3", {
  df <- data.frame(x = 1:2)
  bad_arg <- list("new_col", "x")

  expect_error(
    multi_lut_add(df, bad_arg),
    "Objects need to be list of length 3"
  )
})

test_that("First element of one list is not character", {
  df <- data.frame(x = c("Key1", "Key2"))
  lut <- list(Key1 = 1, Key2 = 2)
  bad_arg <- list(123, "x", lut)

  expect_error(
    multi_lut_add(df, bad_arg),
    "Objects need to be list of length 3"
  )
})

test_that("Second element of one list is not character", {
  df <- data.frame(x = c("Key1", "Key2"))
  lut <- list(Key1 = 1, Key2 = 2)
  bad_arg <- list("new_col", 123, lut)

  expect_error(
    multi_lut_add(df, bad_arg),
    "Objects need to be list of length 3"
  )
})
