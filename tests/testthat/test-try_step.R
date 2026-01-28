# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("try_step executes happy paths correctly", {
  responses <- list(initial = "data")
  logfile <- tempfile()

  res_simple <- try_step(
    step_name = "test_step",
    logic_fn = function() 42,
    rollback_fn = function() NULL,
    responses = responses,
    logfile = logfile
  )
  expect_equal(res_simple, 42)
  expect_type(res_simple, "double")

  res_df <- try_step(
    step_name = "df_step",
    logic_fn = function() data.frame(a = 1:10),
    rollback_fn = function() NULL,
    responses = responses,
    logfile = c()
  )
  expect_s3_class(res_df, "data.frame")
  expect_equal(nrow(res_df), 10)

  res_null <- try_step(
    step_name = "null_step",
    logic_fn = function() NULL,
    rollback_fn = function() NULL,
    responses = responses,
    logfile = NULL
  )
  expect_null(res_null)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("try_step handles argument checks correctly", {
  # Test step_name
  expect_error(
    try_step(123, function() 1, NULL, list(), NULL),
    "must be a non-empty character scalar"
  )
  expect_error(
    try_step(list(), function() 1, NULL, list(), NULL),
    "must be a non-empty character scalar"
  )
  expect_error(
    try_step(c(), function() 1, NULL, list(), NULL),
    "must be a non-empty character scalar"
  )
  # Test logic_fn
  expect_error(
    try_step("studies", list(), NULL, list(), NULL),
    "must be a function"
  )
  expect_error(
    try_step("studies", "foobar", NULL, list(), NULL),
    "must be a function"
  )
  expect_error(
    try_step("studies", NULL, NULL, list(), NULL),
    "must be a function"
  )
  # Test responses
  expect_error(
    try_step("studies", function() 1, NULL, c(), NULL),
    "must be a list"
  )
  expect_error(
    try_step("studies", function() 1, NULL, NULL, NULL),
    "must be a list"
  )
  expect_error(
    try_step("studies", function() 1, NULL, c("foo", "bar"), NULL),
    "must be a list"
  )
  # Test logfile
  expect_error(
    try_step("studies", function() 1, NULL, list(), list()),
    "must be a non-empty character scalar or NULL"
  )
  expect_error(
    try_step("studies", function() 1, NULL, list(), 12345),
    "must be a non-empty character scalar or NULL"
  )
  expect_error(
    try_step("studies", function() 1, NULL, list(), c("foo", "bar")),
    "must be a non-empty character scalar or NULL"
  )
})

test_that("try_step handles error paths and handlers correctly", {
  responses <- list(step1 = "success")
  logfile <- tempfile()

  # Mock the error handler to verify it is called
  local_mocked_bindings(
    workflow_error_handler = function(step, resp, log, rollback) {
      force(step); force(resp); force(rollback) # Ensure captured
      return(function(e) {
        stop(paste0("Handled error in ", step), call. = FALSE)
      })
    }
  )

  # Verify error is caught and passed to handler
  expect_error(
    try_step(
      step_name = "samples",
      logic_fn = function() stop("API Down"),
      rollback_fn = function() "cleaned",
      responses = responses,
      logfile = logfile
    ),
    regexp = "Handled error in samples"
  )

  # Verify rollback_fn is accessible to the handler
  # We use a spy to see if the handler "knows" about the rollback function
  handler_spy <- function(step, resp, log, rollback) {
    expect_equal(step, "rollback_test")
    expect_type(rollback, "closure")
    return(function(e) stop("error"))
  }

  local_mocked_bindings(workflow_error_handler = handler_spy)

  expect_error(
    try_step("rollback_test", function() stop("!"), function() 1, list(), "l"),
    "error"
  )
})
