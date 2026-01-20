example_metadata = "../resources/test_template.xlsx"
example_metadata_analyses = "../resources/test_template_analyses.xlsx"

# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("default_parser processes metadata correctly", {
  result = default_parser(example_metadata, param_file = NULL)

  expect_type(result, "list")
  expect_named(result)
  expect_length(result, 12)
  expect_false("analyses" %in% names(result))
  expect_equal(result$samples$alias, c("Sample1", "Sample2"))
  expect_length(result$files$file, 4)
  expect_equal(
    result$runs$files[[1]],
    c(
      "test_S01_R1.fastq.gz" = "/test_S01_R1.fastq.gz.c4gh",
      "test_S01_R2.fastq.gz" = "/test_S01_R2.fastq.gz.c4gh")
  )
})

test_that("default_parser processes metadata with analyses correctly", {
  result = default_parser(example_metadata_analyses, param_file = NULL)

  expect_type(result, "list")
  expect_named(result)
  expect_length(result, 14)
  expect_true("analyses" %in% names(result))
  expect_equal(result$samples$alias, c("Sample1", "Sample2"))
  expect_length(result$files$file, 4)
  expect_length(result$analysis_files$file, 2)
  expect_equal(
    result$runs$files[[1]],
    c(
      "test_S01_R1.fastq.gz" = "/test_S01_R1.fastq.gz.c4gh",
      "test_S01_R2.fastq.gz" = "/test_S01_R2.fastq.gz.c4gh")
  )
  expect_equal(result$datasets$runs[[1]], c("Run1", "Run2"))
  expect_equal(
    result$analyses$chromosomes[[1]],
    data.frame(id = 1, label = "chr1")
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("default_parser validates input arguments", {
  expect_error(default_parser(123), "must be a non-empty character scalar")
  expect_error(default_parser(list()), "must be a non-empty character scalar")
  expect_error(default_parser(NULL), "must be a non-empty character scalar")


  expect_error(
    default_parser("meta.xlsx", param_file = 123),
    "must be a non-empty character scalar"
  )

  expect_error(
    default_parser("meta.xlsx", param_file = list()),
    "must be a non-empty character scalar"
  )
})
