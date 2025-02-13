# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Standard character vector in 'from' with matching LUT keys", {
  df <- data.frame(
    fruit = c("Apple", "Banana", "Cherry"),
    stringsAsFactors = FALSE
  )

  lut <- list(Apple = "Red", Banana = "Yellow", Cherry = "Red")
  result <- lut_add(df, to = "color", from = "fruit", lut = lut)

  expect_true(!is.list(result$color))
  expect_equal(result$color, c("Red", "Yellow", "Red"))
  expect_equal(nrow(result), 3)
  expect_true("color" %in% names(result))
})

test_that("'From' is a list column", {
  df <- data.frame(id = 1:2, stringsAsFactors = FALSE)
  df$fruits <- list(
    c("Apple", "Cherry"),
    c("Banana", "Cherry")
  )

  lut <- list(Apple = "Red", Banana = "Yellow", Cherry = "Red")
  result <- lut_add(df, to = "colors", from = "fruits", lut = lut)

  expect_true(is.list(result$colors))
  expect_equal(result$colors[[1]], c("Red", "Red"))
  expect_equal(result$colors[[2]], c("Yellow", "Red"))
})

test_that("'from' column is not character", {
  df <- data.frame(x = 1:3, y = 4:6)
  lut <- list("1" = "a", "2" = "b", "3" = "a")
  result <- (lut_add(df, to = "new_col", from = "x", lut = lut))

  expect_equal(result$new_col, c("a", "b", "a"))
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(ncol(result), 3)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("df is not a data frame", {
  not_a_df <- 1:5
  expect_error(
    lut_add(not_a_df, to = "new_col", from = "some_col", lut = list())
  )
})

test_that("'lut' is not a named list", {
  df <- data.frame(key = c("A", "B", "C"))
  invalid_lut <- c("A", "B", "C")

  expect_error(lut_add(df, to = "out", from = "key", lut = invalid_lut))
})

test_that("'from' column does not exist in data frame", {
  df <- data.frame(x = 1:3, y = 4:6)
  lut <- list("1" = "a", "2" = "b", "3" = "c")
  expect_error(lut_add(df, to = "new_col", from = "z", lut = lut))
})

test_that("LUT is missing, NULL or empty", {
  df <- data.frame(x = c("a", "b"))
  expect_error(lut_add(df, to = "out", from = "x", lut = NULL))
  expect_error(lut_add(df, to = "out", from = "x", lut = list()))
})

test_that("Some 'from' values not in LUT", {
  df <- data.frame(item = c("KnownKey", "UnknownKey"), stringsAsFactors = FALSE)
  lut <- list(KnownKey = "MappedValue")

  expect_error(lut_add(df, to = "mapped", from = "item", lut = lut))
})
