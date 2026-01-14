# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".get_rega_key retrieves existing keys", {
  test_var_name <- "TEST_REGA_KEY_123"
  Sys.setenv("TEST_REGA_KEY_123" = "secret_token_abc")
  on.exit(Sys.unsetenv(test_var_name)) # Cleanup

  result <- .get_rega_key(test_var_name)
  expect_equal(result, "secret_token_abc")
  expect_type(result, "character")

  long_key <- paste(rep("x", 1000), collapse = "")
  Sys.setenv("LONG_KEY" = long_key)
  on.exit(Sys.unsetenv("LONG_KEY"), add = TRUE)

  expect_equal(.get_rega_key("LONG_KEY"), long_key)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".get_rega_key handles missing variables and validation", {
  expect_warning(
    {
      res <- .get_rega_key("NON_EXISTENT_VAR_XYZ")
      expect_equal(res, "")
    },
    "No NON_EXISTENT_VAR_XYZ environmental variable found"
  )


  expect_error(.get_rega_key(12345))
  expect_error(.get_rega_key(c("KEY1", "KEY2")))
  expect_error(.get_rega_key(""))
})
