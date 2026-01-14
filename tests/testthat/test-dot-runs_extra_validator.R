# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".runs_extra_validator handles valid values", {
  meta <- list(
    runs = data.frame(
      experiment = c("Experiment1", "Experiment2"),
      alias = c("Sample1", "Sample2"),
      run_file_type = c("fastq", "fastq"),
      files = c("seq1.fastq", "seq2.fastq")
    )
  )

  aliases <- list(
    experiments = list("Experiment1", "Experiment2"),
    samples = list("Sample1", "Sample2")
  )

  result <- .runs_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(2, 7))
  expect_equal(result_summary$passes, rep(2, 7))
  expect_false(all(result_summary$error))
})

test_that(".runs_extra_validator handles logic failures in validation", {
  meta <- list(
    runs = data.frame(
      experiment = c("Experiment1", NA, "Experiment2"),
      alias = c("Sample1", NA, "Sample2"),
      run_file_type = c("fastq", NA, "fastq"),
      files = c("seq1.fastq", "seq1.fastq", NA)
    )
  )

  aliases <- list(
    experiments = list("Experiment3", "Experiment2"),
    samples = list("Sample3", "Sample2")
  )

  result <- .runs_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(3, 7))
  expect_equal(result_summary$fails, c(1, 1, 1, 1, 2, 1, 1))
  expect_equal(result_summary$nNA, c(0, 0, 0, 0, 0, 1, 1))
  expect_false(all(result_summary$error))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".runs_extra_validator handles errors/warnings in validation", {
  meta <- list(
    runs = data.frame(
      experiment = I(list("Experiment1", "Experiment2")),
      alias = data.frame("Sample1")
    )
  )

  aliases <- list(
    experiments = list("Experiment3", "Experiment2"),
    samples = list("Sample3", "Sample2")
  )

  result <- .runs_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, c(2, 1, 0, 0, 0, 2, 0))
  expect_equal(result_summary$passes, c(2, 1, 0, 0, 0, 1, 0))
  expect_equal(result_summary$error, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
})

test_that(".runs_extra_validator rejects invalid input types", {
  expect_error(
    .runs_extra_validator(meta = "not a list", aliases = list()),
    "'meta' must be a list"
  )
})

test_that("runs must be a data.frame", {
  meta <- list(runs = list(experiment = "Experiment 1"))

  expect_error(
    .runs_extra_validator(meta, list(experiments = list())),
    "unable to find an inherited method"
  )
})

test_that("aliases must be a named list", {
  meta <- list(runs = data.frame(experiment = "Experiment 1"))

  expect_error(
    .runs_extra_validator(meta, list()),
    "must be a named list"
  )
})
