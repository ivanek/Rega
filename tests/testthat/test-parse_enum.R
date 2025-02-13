# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Parses a data frame with the default separator '--'", {
  df <- data.frame(
    col1 = c(" a ", "b"),
    col2 = c(" c ", "d ")
  )
  result <- parse_enum(df)
  expect_equal(result, "a--c\nb--d")
})

test_that("Parses a data frame with a custom separator '|' ", {
  df <- data.frame(
    x = c(" hello", "world "),
    y = c(" test ", " code")
  )
  result <- parse_enum(df, sep = "|")
  expect_equal(result, "hello|test\nworld|code")
})

test_that("Parses a data frame with 0 columns", {
  df <- data.frame()
  result <- parse_enum(df)
  expect_equal(result, "")
})

test_that("Parses a data frame with 0 rows", {
  df <- data.frame(
    x = character(0),
    y = character(0)
  )
  result <- parse_enum(df)
  expect_equal(result, "")
})


test_that("Parses a character vector (no trimming needed)", {
  vec <- c("Alpha", "Beta", "Gamma")
  result <- parse_enum(vec)
  expect_equal(result, "Alpha\nBeta\nGamma")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Enum is numeric", {
  num_enum <- c(1, 2, 3)
  expect_error(
    parse_enum(num_enum),
    "Unknown type of enum returned for parsing"
  )
})

test_that("Enum is a list", {
  list_enum <- list("alpha", "beta")
  expect_error(
    parse_enum(list_enum),
    "Unknown type of enum returned for parsing"
  )
})
