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
  expect_true(Rega:::.has_analyses(meta))
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
  expect_true(Rega:::.has_analyses(meta))
})

test_that("'aliases$analyses' not present", {
  meta <- list(
    aliases = list(
      other = list("value")
    ),
    analysis_files = data.frame()
  )
  expect_false(Rega:::.has_analyses(meta))
})

test_that("'aliases$analyses' empty", {
  meta <- list(
    aliases = list(
      other = list("value"), analyses = list()
    ),
    analysis_files = data.frame()
  )
  expect_false(Rega:::.has_analyses(meta))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'meta' is not a list", {
  not_a_list <- "invalid_input"
  expect_error(
    Rega:::.has_analyses(not_a_list),
    "'meta' arguments must have 'aliases' as a list"
  )
})

test_that("'meta$aliases' is missing or not a list", {
  meta <- list()
  expect_error(Rega:::.has_analyses(meta), "must have 'aliases' as a list")
})

# test_that("'meta$aliases$analyses' is not a vector/list", {
#   meta <- list(
#     aliases = list(analyses = data.frame(colA = 1:3)),
#     analysis_files = data.frame(file_path = "file1")
#   )
#   expect_error(
#     Rega:::.has_analyses(meta),
#     "'meta\\$aliases\\$analyses' must be a vector or list"
#   )
# })

test_that("'meta$analysis_files' is missing or not a data frame", {
  meta <- list(
    aliases = list(analyses = c("analysis1", "analysis2"))
  )
  expect_error(Rega:::.has_analyses(meta), "missing value where TRUE/FALSE needed")
})
