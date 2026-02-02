# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_username: keyring", {
  local_mocked_bindings(
    key_list = function(...) {
      data.frame(service = "service_name", username = "keyring_user")
    }
  )

  expect_type(.get_ega_username(), "character")
  expect_identical(.get_ega_username(), "keyring_user")
})

test_that(".get_ega_username: keyring, multiple users, returns first", {
  local_mocked_bindings(
    key_list = function(...) {
      data.frame(
        service = c("service_name1", "service_name2"),
        username = c("keyring_user1", "keyring_user2")
      )
    }
  )

  expect_type(.get_ega_username(), "character")
  expect_identical(.get_ega_username(), "keyring_user1")
})

test_that(".get_ega_username happy paths: envvar (keyring not set)", {
  Sys.setenv(
    "REGA_EGA_USERNAME" = "default_user",
    "TEST_EGA_USER" = "jdoe_ega"
  )
  on.exit(Sys.unsetenv(c("TEST_EGA_USER", "REGA_EGA_USERNAME")))

  result <- .get_ega_username(
    # long random service name
    keyring_name = paste(sample(LETTERS, 100, replace = TRUE), collapse = ""),
    envvar = "TEST_EGA_USER"
  )
  expect_equal(result, "jdoe_ega")
  expect_type(result, "character")
  expect_equal(.get_ega_username(), "default_user")
})

test_that(".get_ega_username falls back to askpass ", {
  local_mocked_bindings(
    askpass = function(prompt) "mock_user_input"
  )

  Sys.unsetenv("EMPTY_VAR")

  result <- .get_ega_username(envvar = "EMPTY_VAR")
  expect_equal(result, "mock_user_input")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_username handles invalid inputs (Error Paths)", {
  expect_error(.get_ega_username(keyring_name = 12345), "must be a non-empty character scalar")
  expect_error(.get_ega_username(envvar = 12345), "must be a non-empty character scalar")
  expect_error(.get_ega_username(keyring_name = c("VAR1", "VAR2")), "must be a non-empty character scalar")
  expect_error(.get_ega_username(envvar = c("VAR1", "VAR2")), "must be a non-empty character scalar")
  expect_error(.get_ega_username(keyring_name = NA_character_), "must be a non-empty character scalar")
  expect_error(.get_ega_username(envvar = NA_character_), "must be a non-empty character scalar")
  expect_error(.get_ega_username(keyring_name = ""), "must be a non-empty character scalar")
  expect_error(.get_ega_username(envvar = ""), "must be a non-empty character scalar")
})

