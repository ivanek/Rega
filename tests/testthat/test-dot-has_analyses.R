# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("'aliases$analyses' is non-empty, analysis_files has at least 1 row => returns TRUE", {
  meta <- list(
    aliases = list(
      analyses = c("analysis1", "analysis2")
    ),
    analysis_files = data.frame(file_path = c("file1", "file2"))
  )
  expect_true(.has_analyses(meta))
})

test_that("'aliases$analyses' length > 0, analysis_files row > 0 => TRUE", {
  meta <- list(
    aliases = list(
      analyses = list("analysisA")
    ),
    analysis_files = data.frame(
      file_name = "myfile",
      stringsAsFactors = FALSE
    )
  )
  expect_true(.has_analyses(meta))
})

test_that("'aliases$analyses' not present", {
  meta <- list(
    aliases = list(
      other = list("value")
    ),
    analysis_files = data.frame()
  )
  expect_false(.has_analyses(meta))
})

test_that("'aliases$analyses' empty", {
  meta <- list(
    aliases = list(
      other = list("value"), analyses = list()
    ),
    analysis_files = data.frame()
  )
  expect_false(.has_analyses(meta))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'meta' is not a list", {
  not_a_list <- "invalid_input"
  expect_error(
    .has_analyses(not_a_list),
    "must be a list"
  )
})

test_that("'meta$aliases' is missing", {
  expect_error(
    .has_analyses(list()),
    "must have a top-level element 'aliases'"
  )

  expect_error(
    .has_analyses(NULL),
    "must be a list"
  )

  expect_error(
    .has_analyses(NULL),
    "must be a list"
  )
})

test_that("'meta$aliases' is not a list", {
  expect_error(
    .has_analyses(list(aliases = c())),
    "'aliases' element within 'meta' must be a list"
  )

  expect_error(
    .has_analyses(list(aliases = NULL)),
    "'aliases' element within 'meta' must be a list"
  )

  expect_error(
    .has_analyses(list(aliases = "abc")),
    "'aliases' element within 'meta' must be a list"
  )
})


test_that("'meta$analysis_files' is missing or not a data frame", {
  meta <- list(
    aliases = list(analyses = c("analysis1", "analysis2"))
  )
  expect_error(.has_analyses(meta), "must contain a top-level element named.*'analysis_files'")
})
