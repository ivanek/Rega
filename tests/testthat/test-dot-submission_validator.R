# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".submission_validator handles valid meta and aliases", {
  meta <- list(
    submission = data.frame(title = "Research Project A")
  )
  aliases <- list(id = "alias_1")

  result <- .submission_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, 1)
  expect_equal(result_summary$passes, 1)
  expect_equal(result_summary$fails, 0)
  expect_false(result_summary$error)
})

test_that(".submission_validator handles boundary cases with NULL aliases", {
  meta <- list(submission = data.frame(title = "Clinical Trial"))

  result <- .submission_validator(meta, NULL)
  result_summary <- summary(result)
  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, 1)
  expect_equal(result_summary$passes, 1)
})

test_that(".submission_validator catches logic failures in validation", {
  meta <- list(
    submission = data.frame(title = NA)
  )

  result <- .submission_validator(meta, list())
  result_summary <- summary(result)

  expect_equal(result_summary$items, 1)
  expect_equal(result_summary$fails, 1)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".submission_validator catches errors/warnings in validation", {
  meta <- list(
    submission = data.frame(title = NULL)
  )

  result <- .submission_validator(meta, list())
  result_summary <- summary(result)

  expect_equal(result_summary$items, 1)
  expect_equal(result_summary$passes, 1)
  expect_true(result_summary$warning)
})

test_that(".submission_validator rejects invalid input types", {
  expect_error(
    .submission_validator(meta = "not a list", aliases = list()),
    "'meta' must be a list"
  )
})

test_that("submission must be a data.frame", {
  meta <- list(submission = list(title = "Clinical Trial"))

  expect_error(
    .submission_validator(meta, NULL),
    "unable to find an inherited method"
  )
})
