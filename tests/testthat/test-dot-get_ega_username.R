# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_password: keyring", {
  skip_if_not(keyring::has_keyring_support(), "Keyring support not available")

  local_mocked_bindings(
    key_get = function(...) "keyring_secret_pass"
  )

  expect_type(.get_ega_password(), "character")
  expect_identical(.get_ega_password(), "keyring_secret_pass")
})

test_that(".get_ega_username: keyring, multiple users, returns first", {
  skip_if_not(keyring::has_keyring_support(), "Keyring support not available")

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

test_that(".get_ega_username happy paths: envvar (keyring name not set)", {
  skip_if_not(keyring::has_keyring_support(), "Keyring support not available")

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

test_that(".get_ega_username happy paths: keyring support not available", {
  skip_if(keyring::has_keyring_support(), "Run when only 'env' backends are available")

  Sys.setenv(
    "REGA_EGA_USERNAME" = "default_user",
    "TEST_EGA_USER" = "jdoe_ega"
  )
  on.exit(Sys.unsetenv(c("TEST_EGA_USER", "REGA_EGA_USERNAME")))

  expect_warning(
    result <- .get_ega_username(
      envvar = "TEST_EGA_USER"
    )
  )

  expect_equal(result, "jdoe_ega")
  expect_type(result, "character")
  expect_warning(
    expect_equal(.get_ega_username(), "default_user")
  )
})

test_that(".get_ega_username falls back to askpass", {
  skip_if_not(keyring::has_keyring_support(), "Keyring support not available")
  local_mocked_bindings(
    askpass = function(prompt) "mock_user_input"
  )

  Sys.unsetenv("EMPTY_VAR")
  expect_equal(.get_ega_username(envvar = "EMPTY_VAR"), "mock_user_input")
})

test_that(".get_ega_username: askpass fallback", {
  skip_if(keyring::has_keyring_support(), "Run when only 'env' backends are available")

  local_mocked_bindings(
    askpass = function(prompt) "mock_user_input"
  )

  Sys.unsetenv("EMPTY_VAR")
  expect_warning(
    expect_equal(.get_ega_username(envvar = "EMPTY_VAR"), "mock_user_input")
  )
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

