# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".dataset_analyses_validator handles valid values", {
  meta <- list(
    datasets = data.frame(
      analyses = c("Analysis1", "Analysis2")
    ),
    analyses = list()
  )

  aliases <- list(
    analyses = list("Analysis1", "Analysis2")
  )

  result <- .dataset_analyses_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, 2)
  expect_equal(result_summary$passes, 2)
  expect_false(all(result_summary$error))
})

test_that(".dataset_analyses_validator handles logic failures in validation", {
  meta <- list(
    datasets = data.frame(
      analyses = c("Analysis1", "Analysis3", NA)
    ),
    analyses = list()
  )

  aliases <- list(
    analyses = list("Analysis1", "Analysis2")
  )

  result <- .dataset_analyses_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, 3)
  expect_equal(result_summary$passes, 1)
  expect_equal(result_summary$fails, 1)
  expect_equal(result_summary$nNA, 1)
  expect_false(all(result_summary$error))
})

test_that("no analyses in meta", {
  meta <- list(datasets = list(experiment = "Experiment 1"))

  expect_null(.dataset_analyses_validator(meta, list(analyses = list())))
})


# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".dataset_analyses_validator handles errors in validation", {
  meta <- list(
    datasets = data.frame(
      analyses = list("Analysis1", "Analysis3")
    ),
    analyses = list()
  )

  aliases <- list(
    analyses = list("Analysis1", "Analysis2")
  )

  result <- .dataset_analyses_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$error, TRUE)
})

test_that(".dataset_analyses_validator rejects invalid input types", {
  expect_error(
    .dataset_analyses_validator(meta = "not a list", list(analyses = list())),
    "'meta' must be a list"
  )
})

test_that("datasets must be a data.frame", {
  meta <- list(datasets = list(experiment = "Experiment 1"), analyses = list())

  expect_error(
    .dataset_analyses_validator(meta, list(analyses = list())),
    "unable to find an inherited method"
  )
})

test_that("aliases must be a named list", {
  meta <- list(
    datasets = data.frame(
      analyses = c("Analysis1", "Analysis2")
    ),
    analyses = list()
  )

  expect_error(
    .dataset_analyses_validator(meta, c()),
    "must be a named list"
  )
})
