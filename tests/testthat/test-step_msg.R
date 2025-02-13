# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Function works for a single step", {
  fn <- step_msg(1)
  expect_message(fn("All done."), "Step 1/1 - All done.")
})

test_that("Function increments step count properly", {
  fn <- step_msg(3)

  expect_message(fn("Starting..."), "Step 1/3 - Starting...")
  expect_message(fn("Working..."), "Step 2/3 - Working...")
  expect_message(fn("Finished!"), "Step 3/3 - Finished!")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Missing 'steps' argument", {
  expect_error(step_msg()("test"), "argument \"steps\" is missing")
})

test_that("'steps' is zero", {
  expect_error(step_msg(0), "'steps' must be a positive integer")
})

test_that("'steps' is not numeric", {
  expect_error(step_msg("three"), "'steps' must be numeric of length 1.")
})
