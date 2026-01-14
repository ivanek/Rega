# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".validate_character_scalar accepts valid strings", {
  expect_true(.validate_character_scalar("hello"))

  expect_true(.validate_character_scalar("data", varname = "input_field"))

  large_string <- paste(rep("A", 1e4), collapse = "")
  expect_true(.validate_character_scalar(large_string))
  expect_true(.validate_character_scalar("!@#$%^&*()"))

  expect_type(.validate_character_scalar("test"), "logical")
})

test_that(".validate_character_scalar correctly deparses variable names in errors", {
  my_custom_variable <- 5 # Not a character

  expect_error(
    .validate_character_scalar(my_custom_variable),
    "my_custom_variable must be a non-empty character scalar."
  )

  expect_error(
    .validate_character_scalar(5, varname = "OverrideName"),
    "OverrideName must be a non-empty character scalar."
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".validate_character_scalar rejects invalid content", {
  expect_error(.validate_character_scalar(""), "must be a non-empty character scalar")

  expect_error(.validate_character_scalar(NA_character_), "must be a non-empty character scalar")

  expect_error(.validate_character_scalar(123), "must be a non-empty character scalar")
  expect_error(.validate_character_scalar(TRUE), "must be a non-empty character scalar")

  expect_error(.validate_character_scalar(c("a", "b")), "must be a non-empty character scalar")
})
