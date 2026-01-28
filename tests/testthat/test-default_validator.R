minimal_metadata <- list(
  aliases = list(
    studies = "Study1", experiments = "Experiment1",
    datasets = "Dataset1", samples = "Sample1", runs = "Run1",
    analyses = "Analysis1"
  ),
  files = tibble::tibble(
    file = "raw.fastq.gz", ega_file = list("raw.fastq.gz.c4gh")
  ),
  submission = tibble::tibble(title = "Submission"),
  studies = tibble::tibble(
    study = "Study1", title = "Study Title for bulk RNASeq experiment",
    description = "Study Description. Lorem ipsum dolor sit amet? consectetur adipiscing elit.",
    study_type = "Whole Genome Sequencing"
  ),
  samples = tibble::tibble(
    alias = "Sample1", phenotype = "wild-type",
    biological_sex = "female", subject_id = "ID1"
  ),
  experiments = tibble::tibble(
    study = "Study1", experiment = "Experiment1",
    design_description = "Experiment Design",
    library_selection = "RANDOM", instrument_model_id = 1L,
    library_layout = "SINGLE", library_strategy = "WGS",
    library_source = "GENOMIC"
  ),
  runs = tibble::tibble(
    run = "Run1", experiment = "Experiment1", run_file_type = "srf",
    alias = "Sample1", files = list("raw.fastq.gz.c4gh")
  ),
  datasets = tibble::tibble(
    dataset = "Dataset1", title = "Dataset Title for bulk RNASeq experiment",
    description = "Dataset Description. Lorem ipsum dolor sit amet? consectetur adipiscing elit.",
    policy_accession_id = "EGAP00000000001",
    dataset_types = list("Whole genome sequencing"),
    runs = list("Run1")
  )
)

# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("default_validator processes metadata successfully", {
  result <- default_validator(minimal_metadata)

  expect_s3_class(result, "data.frame")
  expect_equal(result$items, rep(1, 38))
  expect_equal(result$passes, rep(1, 38))
  expect_false(all(result$error))
})

test_that("default_validator handles NULL validation results gracefully", {
  local_mocked_bindings(
    .basic_validator = function(...) NULL,
    .submission_validator = function(...) NULL,
    .runs_extra_validator = function(...) NULL,
    .studies_extra_validator = function(...) NULL,
    .datasets_extra_validator = function(...) NULL,
    .analyses_extra_validator = function(...) NULL,
    .dataset_analyses_validator = function(...) NULL,
    .summarise_validation = function(vs) NULL
  )

  result <- default_validator(list(aliases = list()))
  expect_null(result)
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("default_validator catches alias-related logic failures", {
  empty_meta <- list(not_aliases = list())
  expect_error(
    default_validator(empty_meta),
    "Aliases are not specified"
  )

  expect_error(
    default_validator(list(submission = data.frame()), aliases = list(1)),
    "'aliases' must be a named list"
  )

  expect_error(
    default_validator(list(submission = data.frame()), aliases = c("a")),
    "'aliases' must be a named list"
  )
})
