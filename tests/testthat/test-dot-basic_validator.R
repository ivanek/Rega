# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Code_list_entry is present in meta => returns a validation object", {
  meta <- list(
    colors = data.frame(
      color = c("red", "blue", "green"),
      stringsAsFactors = FALSE
    )
  )
  aliases <- list(colors = c("red", "blue", "green", "yellow"))

  result <- Rega:::.basic_validator(
    meta,
    aliases,
    column = "color",
    code_list_entry = "colors"
  )

  result_summary <- validate::summary(result)

  expect_s4_class(result, "validation")
  expect_equal(nrow(result_summary), 4)
  expect_equal(
    result_summary$name,
    c("color_is_na", "color_is_unique", "color_in_aliases", "color_all_aliases")
  )
  expect_equal(result_summary$items, c(3, 3, 3, 4))
  expect_equal(result_summary$passes, c(3, 3, 3, 3))
  expect_equal(result_summary$fails, c(0, 0, 0, 1))
  expect_equal(
    result_summary$expression,
    c(
      "!is.na(color)", "is_unique(color)", "color %vin% aliases[[\"colors\"]]",
      "aliases[[\"colors\"]] %vin% color"
    )
  )
})

test_that("Uses custom column and code list, all present", {
  meta <- list(
    my_codes = data.frame(my_col = c("A", NA, "C", "D", "D", "E"))
  )
  aliases <- list(my_codes = c("A", "B", "C", "D", "G"))

  result <- Rega:::.basic_validator(
    meta, aliases,
    column = "my_col", code_list_entry = "my_codes"
  )
  result_summary <- validate::summary(result)

  expect_s4_class(result, "validation")
  expect_equal(nrow(result_summary), 4)
  expect_equal(
    result_summary$name,
    c("my_col_is_na", "my_col_is_unique", "my_col_in_aliases", "my_col_all_aliases")
  )
  expect_equal(result_summary$items, c(6, 6, 6, 5))
  expect_equal(result_summary$passes, c(5, 4, 4, 3))
  expect_equal(result_summary$fails, c(1, 2, 1, 0))
  expect_equal(result_summary$nNA, c(0, 0, 1, 2))
  expect_equal(
    result_summary$expression,
    c(
      "!is.na(my_col)", "is_unique(my_col)", "my_col %vin% aliases[[\"my_codes\"]]",
      "aliases[[\"my_codes\"]] %vin% my_col"
    )
  )
})

test_that("'code_list_entry' not present in meta => returns NULL", {
  meta <- list(
    shapes = data.frame(
      color = c("circle", "triangle"),
      stringsAsFactors = FALSE
    )
  )
  aliases <- list(colors = c("red", "blue"))

  # 'code_list_entry' = "colors" but meta has only "shapes"
  result <- Rega:::.basic_validator(
    meta, aliases,
    column = "color", code_list_entry = "colors"
  )
  expect_null(result)
})

test_that("'column' not present in meta", {
  meta <- list(
    colors = data.frame(
      shape = c("circle", "triangle"),
      stringsAsFactors = FALSE
    )
  )
  aliases <- list(colors = c("red", "blue"))

  # 'code_list_entry' = "colors" but meta has only "shapes"
  result <- Rega:::.basic_validator(
    meta, aliases,
    column = "color", code_list_entry = "colors"
  )
  result_summary <- validate::summary(result)

  expect_s4_class(result, "validation")
  expect_equal(nrow(result_summary), 4)
  expect_equal(result_summary$items, c(0, 0, 0, 0))
  expect_equal(result_summary$passes, c(0, 0, 0, 0))
  expect_equal(result_summary$fails, c(0, 0, 0, 0))
  expect_equal(result_summary$nNA, c(0, 0, 0, 0))
  expect_equal(result_summary$error, c(TRUE, TRUE, TRUE, TRUE))
})


test_that("'aliases' is empty", {
  meta <- list(
    codes = data.frame(code = c("X", "Y", "Z"))
  )
  aliases <- list()

  result <- Rega:::.basic_validator(
    meta, aliases,
    column = "code", code_list_entry = "codes"
  )
  result_summary <- validate::summary(result)

  expect_s4_class(result, "validation")
  expect_equal(nrow(result_summary), 4)
  expect_equal(
    result_summary$name,
    c("code_is_na", "code_is_unique", "code_in_aliases", "code_all_aliases")
  )
  expect_equal(result_summary$items, c(3, 3, 3, 0))
  expect_equal(result_summary$passes, c(3, 3, 0, 0))
  expect_equal(result_summary$fails, c(0, 0, 3, 0))
})


# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Meta[[code_list_entry]] is not a data frame => confront() fails", {
  meta <- list(
    colors = 1:5
  )
  aliases <- list(colors = c("red", "blue"))

  # confront() expects a data frame or similar table as the first argument
  expect_error(
    Rega:::.basic_validator(
      meta, aliases,
      column = "color", code_list_entry = "colors"
    ),
    "unable to find an inherited method for function"
  )
})
