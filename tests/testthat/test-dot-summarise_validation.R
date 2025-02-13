# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("No issues => prints 'Validation passed!'", {
  val_sum <- data.frame(
    fails   = c(0, 0),
    nNA     = c(0, 0),
    error   = c(0, 0),
    warning = c(0, 0)
  )

  expect_message(
    Rega:::.summarise_validation(val_sum),
    "Validation passed!"
  )
})

test_that("Some fails => prints 'Validation failed!' message with correct counts", {
  val_sum <- data.frame(
    fails   = c(1, 0, 2),
    nNA     = c(1, 0, 1),
    error   = c(0, 0, 0),
    warning = c(0, 0, 0)
  )

  expect_message(
    Rega:::.summarise_validation(val_sum),
    "Validation failed! See validation object for details:"
  )
  expect_message(Rega:::.summarise_validation(val_sum), "Fails: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "NAs: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "Errors: 0")
  expect_message(Rega:::.summarise_validation(val_sum), "Warnings 0")
})

test_that("Multiple fails, nas, errors, warnings => combined 'Validation failed!'", {
  val_sum <- data.frame(
    fails   = c(1, 2, 0),
    nNA     = c(0, 1, 1),
    error   = c(1, 0, 1),
    warning = c(0, 0, 1)
  )

  expect_message(
    Rega:::.summarise_validation(val_sum),
    "Validation failed! See validation object for details:"
  )
  expect_message(Rega:::.summarise_validation(val_sum), "Fails: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "NAs: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "Errors: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "Warnings 1")
})

test_that("'validation_summary' is a list", {
  val_sum <- list(fails = c(1, 0), nNA = 0, error = c(1, 1, 0), warning = 1)

  expect_message(
    Rega:::.summarise_validation(val_sum),
    "Validation failed! See validation object for details:"
  )
  expect_message(Rega:::.summarise_validation(val_sum), "Fails: 1")
  expect_message(Rega:::.summarise_validation(val_sum), "NAs: 0")
  expect_message(Rega:::.summarise_validation(val_sum), "Errors: 2")
  expect_message(Rega:::.summarise_validation(val_sum), "Warnings 1")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'validation_summary' lacks required columns", {
  val_sum <- data.frame(
    # 'fails' column missing
    nNA     = c(1, 0),
    error   = c(0, 1),
    warning = c(0, 0)
  )

  expect_error(
    Rega:::.summarise_validation(val_sum),
    "Missing required columns in 'validation_summary'"
  )
})

test_that("Columns are not numeric => sum(...) might fail or misbehave", {
  val_sum <- data.frame(
    fails = c("a", "b"),
    nNA = c("c", "d"),
    error = c("e", "f"),
    warning = c("g", "h"),
    stringsAsFactors = FALSE
  )

  expect_error(
    Rega:::.summarise_validation(val_sum),
    "invalid 'type' \\(character\\) of argument"
  )
})
