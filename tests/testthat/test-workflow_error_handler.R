# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Handler returns a closure and includes correct message for 'submission'", {
  h <- workflow_error_handler(
    step = "submission",
    responses = list(a = 1),
    logfile = NULL
  )

  expect_true(is.function(h))

  expect_message(
    expect_error(
      tryCatch(
        {
          stop("Test error")
        },
        error = h
      ),
      "Test error"
    ), "Error while creating submission"
  )
})

test_that("Handler includes extra message for non-'submission' step, logs data", {
  tmp_file <- tempfile(fileext = ".yaml")
  h <- workflow_error_handler(
    step = "analysis",
    responses = list(x = 123),
    logfile = tmp_file
  )

  expect_message(
    expect_error(
      tryCatch(
        {
          stop("Some other error")
        },
        error = h
      ),
      "Some other error"
    ), "Error while creating analysis\\. All analysis were removed."
  )

  expect_true(file.exists(tmp_file))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Error path: 'step' is not a character string", {
  expect_error(
    workflow_error_handler(123, responses = list(), logfile = NULL),
    "'step' must be a character"
  )
})


test_that("Captured expressions cause an error themselves", {
  h <- workflow_error_handler(
    step = "analysis",
    responses = list(),
    logfile = NULL,
    zzz + 1
  )
  expect_message(
    expect_error(
      tryCatch(stop("Initial error"), error = h),
      "object 'zzz' not found"
    )
  )
})
