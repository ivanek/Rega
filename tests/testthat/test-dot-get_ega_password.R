# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_password happy paths", {
  Sys.setenv(REGA_EGA_PASSWORD = "plaintext_pass", REGA_KEY = "")
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))

  expect_warning(
    expect_warning(
      res <- .get_ega_password(),
    )
  )

  expect_equal(res, "plaintext_pass")
  expect_type(res, "character")

  rega_key <- httr2::secret_make_key()
  ega_password <- "encrypted_blob"
  enc_password <- httr2::secret_encrypt(ega_password, rega_key)

  Sys.setenv(REGA_EGA_PASSWORD = enc_password, REGA_KEY = rega_key)
  res_decrypted <- .get_ega_password()
  expect_equal(res_decrypted, "encrypted_blob")
})

test_that(".get_ega_password falls back to askpass ", {
  askpass <- function(prompt) "mock_user_input"

  local_mocked_bindings(
    askpass = askpass,
    .package = "Rega"
  )

  Sys.setenv(REGA_EGA_PASSWORD = "", REGA_KEY = "")
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))

  expect_warning(
    res_prompt <- .get_ega_password(),
    "connect via unecrypted password"
  )

  expect_equal(res_prompt, "mock_user_input")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".get_ega_password input and logic failures", {
  Sys.setenv(REGA_EGA_PASSWORD = "data", REGA_KEY = httr2::secret_make_key())
  on.exit(Sys.unsetenv(c("REGA_EGA_PASSWORD", "REGA_KEY")))

  expect_error(.get_ega_password(envvar = 12345))
  expect_error(.get_ega_password(envvar = c("PASS1", "PASS2")))

  expect_false(identical(.get_ega_password(), "data"))
})
