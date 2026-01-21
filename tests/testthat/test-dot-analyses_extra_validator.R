# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".analyses_extra_validator handles valid values", {
  meta <- list(
    analyses = data.frame(
      title = c("Analysis Title 1", "Analysis Title 2"),
      description = c(
        "Lorem ipsum! dolor sit amet. consectetur adipiscing elit.",
        "Nullam tincidunt! vulputate porttitor. Nam at pulvinar risus"
      ),
      samples = c("Sample1", "Sample2"),
      experiments = c("Experiment1", "Experiment2"),
      files = c("seq1.bam", "seq2.bam")
    )
  )

  aliases <- list(
    experiments = list("Experiment1", "Experiment2"),
    samples = list("Sample1", "Sample2")
  )

  result <- .analyses_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(2, 8))
  expect_equal(result_summary$passes, rep(2, 8))
  expect_false(all(result_summary$error))
})


test_that(".analyses_extra_validator handles logic failures in validation", {
  meta <- list(
    analyses = data.frame(
      title = c("Title1", "Analysis Title 2", NA),
      description = c(
        "Lorem ipsum! dolor sit amet. consectetur adipiscing elit.",
        "Description 2",
        NA
      ),
      samples = c("Sample1", "Sample2", NA),
      experiments = c("Experiment1", "Experiment2", NA),
      files = c("seq1.bam", "seq1.bam", NA)
    )
  )

  aliases <- list(
    experiments = list("Experiment1", "Experiment3"),
    samples = list("Sample1", "Sample3")
  )

  result <- .analyses_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(3, 8))
  expect_equal(result_summary$passes, c(3, 3, 1, 1, 1, 1, 2, 1))
  expect_equal(result_summary$fails, c(0, 0, 2, 1, 1, 1, 1, 2))
  expect_equal(result_summary$nNA, c(0, 0, 0, 1, 1, 1, 0, 0))
  expect_false(all(result_summary$error))
})

test_that("no analyses in meta", {
  meta <- list(runs = data.frame(experiment = "Experiment 1"))

  expect_null(.analyses_extra_validator(meta, list(samples = list())))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".analyses_extra_validator handles errors/warnings in validation", {
  meta <- list(
    analyses = data.frame(
      title = list("Title1", "Title2"),
      description = data.frame("Description1", "Description2"),
      samples = I(list(NULL, NULL)),
      experiments = c("Experiment1", "Experiment2")
    )
  )

  aliases <- list(
    experiments = list("Experiment3", "Experiment2"),
    samples = list("Sample3", "Sample2")
  )

  result <- .analyses_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, c(0, 0, 0, 0, 0, 2, 0, 0))
  expect_equal(result_summary$passes, c(0, 0, 0, 0, 0, 1, 0, 0))
  expect_equal(result_summary$fails, c(0, 0, 0, 0, 0, 1, 0, 0))
  expect_equal(result_summary$error, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that(".analyses_extra_validator rejects invalid input types", {
  expect_error(
    .analyses_extra_validator(meta = "not a list", aliases = list()),
    "'meta' must be a list"
  )
})

test_that("analyses must be a data.frame", {
  meta <- list(analyses = list(experiment = "Experiment 1"))

  expect_error(
    .analyses_extra_validator(meta, list(samples = list())),
    "unable to find an inherited method"
  )
})

test_that("aliases must be a named list", {
  meta <- list(analyses = data.frame(experiment = "Experiment 1"))

  expect_error(
    .analyses_extra_validator(meta, list()),
    "must be a named list"
  )
})
