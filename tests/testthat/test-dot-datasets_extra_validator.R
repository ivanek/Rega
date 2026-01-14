# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".datasets_extra_validator handles valid values", {
  meta <- list(
    datasets = data.frame(
      title = c(
        "Dataset for analysis of bulk RNASeq of cells",
        "Dataset for analysis of multiome from patient samples"
      ),
      description = c(
        paste0(
          "Lorem ipsum dolor sit amet. Consectetur adipiscing eli?",
          "Duis eu accumsan lorem! porta tristique lacus"
        ),
        paste0(
          "Cras ante lectus. tincidunt at tempus eget! dictum ut massa.",
          "Aenean fermentum vitae velit in pharetra?"
        )
      ),
      runs = c("Run1", "Run2")
    )
  )

  aliases <- list(
    runs = list("Run1", "Run2")
  )

  result <- .datasets_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(2, 6))
  expect_equal(result_summary$passes, rep(2, 6))
  expect_false(all(result_summary$error))
})

test_that(".datasets_extra_validator handles logic failures in validation", {
  meta <- list(
    datasets = data.frame(
      title = c(
        "Dat",
        "Dataset for samples",
        "Dat"
      ),
      description = c(
        "",
        "Cras ante lectus. tincidunt at tempus eget!",
        ""
      ),
      runs = c("Run1", "Run2", NA)
    )
  )

  aliases <- list(
    runs = list("Run1", "Run3", "Run4")
  )

  result <- .datasets_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(3, 6))
  expect_equal(result_summary$passes, c(1, 1, 1, 1, 1, 0))
  expect_equal(result_summary$fails, c(2, 2, 1, 0, 2, 3))
  expect_equal(result_summary$nNA, c(0, 0, 1, 2, 0, 0))
  expect_false(all(result_summary$error))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".datasets_extra_validator handles logic failures in validation", {
  meta <- list(
    datasets = data.frame(
      title = list(
        "Dataset for analysis of bulk RNASeq of cells"
      ),
      description = data.frame(
        "Cras ante lectus. tincidunt at tempus eget!"
      ),
      runs = c("Run1")
    )
  )

  aliases <- list(
    runs = list("Run1", "Run3", "Run4")
  )

  result <- .datasets_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, c(0, 0, 1, 3, 0, 0))
  expect_equal(result_summary$passes, c(0, 0, 1, 1, 0, 0))
  expect_equal(result_summary$fails, c(0, 0, 0, 2, 0, 0))
  expect_equal(result_summary$error, c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})


test_that(".datasets_extra_validator rejects invalid input types", {
  expect_error(
    .datasets_extra_validator(meta = "not a list", aliases = list()),
    "'meta' must be a list"
  )
})

test_that("analyses must be a data.frame", {
  meta <- list(datasets = list(experiment = "Experiment 1"))

  expect_error(
    .datasets_extra_validator(meta, list(samples = list())),
    "unable to find an inherited method"
  )
})

test_that("aliases must be a named list", {
  meta <- list(datasets = data.frame(experiment = "Experiment 1"))

  expect_error(
    .datasets_extra_validator(meta, list()),
    "must be a named list"
  )
})
