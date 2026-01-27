# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_password: keyring", {
  local_mocked_bindings(
    key_get = function(...) "keyring_secret_pass"
  )

  res <- .get_ega_password()
  expect_type(res, "character")
  expect_identical(res, "keyring_secret_pass")
})

test_that(".get_ega_password: ennvar with empty keyring", {
  local_mocked_bindings(
    key_get = function(...) charcter(0)
  )

  rega_key <- httr2::secret_make_key()
  ega_password <- "encrypted_blob"
  enc_password <- httr2::secret_encrypt(ega_password, rega_key)

  Sys.setenv(REGA_EGA_PASSWORD = enc_password, REGA_KEY = rega_key)
  res_decrypted <- .get_ega_password()
  expect_equal(res_decrypted, "encrypted_blob")
})

test_that(".get_ega_password falls back to askpass", {
  local_mocked_bindings(
    key_get = function(...) stop("not found"),
    askpass = function(...) "mock_user_input"
  )

  Sys.setenv(REGA_EGA_PASSWORD = "", REGA_KEY = "tFNO-nkNezlV-uRQRy83gg")
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))

  res_prompt <- .get_ega_password()

  expect_equal(res_prompt, "mock_user_input")
})

test_that(".get_ega_password logic: passing ennvar to .get_rega_key", {
  secret_key = httr2::secret_make_key()
  Sys.setenv(REGA_EGA_PASSWORD = "unused_password")
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD")))

  passed_arg <- NULL

  local_mocked_bindings(
    key_get = function(service) stop("not found"),
    .get_rega_key = function(...) {
      args <- list(...)
      passed_arg <<- args
      secret_key
    }
  )

  .get_ega_password(secret_envvar = "NEW_REGA_KEY")
  expect_equal(passed_arg$secret_envvar, "NEW_REGA_KEY")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_password plaintext password error", {
  Sys.setenv(REGA_EGA_PASSWORD = "plaintext_pass", REGA_KEY = "")
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))
  expect_error(.get_ega_password(
    keyring_name = paste(sample(LETTERS, 100, replace = TRUE), collapse = ""),
  ),
  "unecrypted password is not permitted")
})

test_that(".get_ega_password input and logic failures", {
  Sys.setenv(REGA_EGA_PASSWORD = "data", REGA_KEY = httr2::secret_make_key())
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))
  expect_false(identical(.get_ega_password(), "data"))

  expect_error(.get_ega_password(keyring_name = 12345), "must be a non-empty character scalar")
  expect_error(.get_ega_password(envvar = 12345), "must be a non-empty character scalar")
  expect_error(.get_ega_password(keyring_name = c("VAR1", "VAR2")), "must be a non-empty character scalar")
  expect_error(.get_ega_password(envvar = c("VAR1", "VAR2")), "must be a non-empty character scalar")
  expect_error(.get_ega_password(keyring_name = NA_character_), "must be a non-empty character scalar")
  expect_error(.get_ega_password(envvar = NA_character_), "must be a non-empty character scalar")
  expect_error(.get_ega_password(keyring_name = ""), "must be a non-empty character scalar")
  expect_error(.get_ega_password(envvar = ""), "must be a non-empty character scalar")
})
