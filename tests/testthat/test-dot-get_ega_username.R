# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

# TODO test the empty envvar by mocking user interaction with popup window
test_that(".get_ega_username retrieves from environment ", {
  test_var <- "TEST_EGA_USER"
  Sys.setenv("TEST_EGA_USER" = "jdoe_ega")
  on.exit(Sys.unsetenv(test_var))

  result <- .get_ega_username(test_var)
  expect_equal(result, "jdoe_ega")
  expect_type(result, "character")

  Sys.setenv("REGA_EGA_USERNAME" = "default_user")
  on.exit(Sys.unsetenv("REGA_EGA_USERNAME"), add = TRUE)

  expect_equal(.get_ega_username(), "default_user")
})

test_that(".get_ega_username falls back to askpass ", {
  askpass <- function(prompt) "mock_user_input"

  local_mocked_bindings(
    askpass = askpass,
    .package = "Rega"
  )

  Sys.unsetenv("EMPTY_VAR")

  result <- .get_ega_username(envvar = "EMPTY_VAR")
  expect_equal(result, "mock_user_input")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_username handles invalid inputs (Error Paths)", {
  expect_error(.get_ega_username(12345), "must be a non-empty character scalar")
  expect_error(.get_ega_username(c("VAR1", "VAR2")))
  expect_error(.get_ega_username(NA_character_))
  expect_error(.get_ega_username(""))
})
