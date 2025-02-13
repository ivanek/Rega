# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Simple table organization with NA columns, removes whitespace", {
  tab <- data.frame(
    A = c("H1", "H2", "H3"), B = c("  Value1", "Name1", " 15"),
    C = c(NA, NA, NA), D = c("H1", "H2", "H3"),
    E = c("Value2", "Name2  ", " 25 "), F = NA
  )

  result <- row_table_formatter(tab, NULL)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("h1", "h2", "h3"))
  expect_equal(result$h1, c("Value1", "Value2"))
  expect_equal(result$h2, c("Name1", "Name2"))
  expect_equal(result$h3, c("15", "25"))
})

test_that("Table with folding, removes whitespace", {
  tab <- data.frame(
    A = c("H1", "H2", "H3", "H2_1", "H2_3"),
    B = c("Value1 ", " Name1 ", "15", "Name3   ", NA),
    D = c("H1", "H2", "H3", NA, NA),
    E = c("Value2", "  Name2", "25", NA, NA)
  )

  result <- row_table_formatter(tab, list(fold = c("h2")))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("h1", "h3", "h2"))
  expect_equal(result$h1, c("Value1", "Value2"))
  expect_equal(result$h2, list(c("Name1", "Name3"), "Name2"))
  expect_equal(result$h3, c("15", "25"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Odd number of columns after NA removal", {
  tab <- data.frame(
    A = c("H1", "H2", "H3"), C = c(NA, NA, NA),
    D = c("H1", "H2", "H3"), E = c("Value2", "Name2  ", " 25 "), F = NA
  )

  expect_error(
    row_table_formatter(tab, list(fold = c("h2"))),
    "Incorrect column organization of the table columns"
  )
})

test_that("Empty table", {
  tab <- data.frame()
  expect_error(
    row_table_formatter(tab, NULL),
    "Incorrect column organization of the table columns"
  )
})

test_that("Non-data frame input", {
  tab <- list(A = c("Header1", "A1"), B = c("Value1", "B1"))
  expect_error(
    row_table_formatter(tab, NULL),
    "must be an array of at least two dimensions"
  )
})

test_that("Mismatched column names in split tables", {
  tab <- data.frame(
    A = c("H1", "H2"), B = c("Value1", "B1"),
    C = c("H1", "H3"), D = c("Value2", "B2")
  )
  expect_error(
    row_table_formatter(tab, NULL),
    "undefined columns selected"
  )
})
