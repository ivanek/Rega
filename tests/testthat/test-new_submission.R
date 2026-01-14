minimal_metadata <- list(
  aliases = list(
    studies = "Study1", experiments = "Experiment1",
    datasets = "Dataset1", samples = "Sample1", runs = "Run1",
    analyses = "Analysis1"
  ),
  files = tibble::tibble(
    file = "raw.fastq.gz", ega_file = "raw.fastq.gz.c4gh"
  ),
  analysis_files = tibble::tibble(
    file = "processed.bam", ega_file = list("processed.bam.c4gh")
  ),
  submission = tibble::tibble(title = "Submission"),
  studies = tibble::tibble(
    study = "Study1", title = "Study Title",
    description = "Study Description",
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
  analyses = tibble::tibble(
    study = "Study1", analysis = "Analysis1",
    title = "Analysis title", descrption = "Analysis description",
    analysis_types = "REFERENCE ALIGNMENT", files = list("processed.bam.c4gh"),
    experiments = "Experiment1", samples = "Sample1"
  ),
  datasets = tibble::tibble(
    dataset = "Dataset1", title = "Dataset Title",
    description = "Dataset Description",
    policy_accession_id = "EGAP00000000001",
    dataset_types = list("Whole genome sequencing"),
    runs = list("Run1"), analyses = "Analysis1"
  )
)

# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("successfully completes a full submission workflow", {
  mock_client <- list(
    get__samples = function() data.frame(alias = "Unique_sample_123"),
    get__files = function(prefix) data.frame(provisional_id = 12345, stringsAsFactors = FALSE),
    post__submissions = function(body) list(provisional_id = 12345)
  )

  local_mocked_bindings(
    create_client = function(...) mock_client,
    get_or_post = function(id, data, client, type, ...) {
      data$provisional_id <- 12345
      data
    }
  )

  result <- new_submission(request_data = minimal_metadata)

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "raw_files", "submission", "samples", "studies",
      "experiments", "runs", "analysis_files", "analyses", "datasets"
    )
  )

  expect_equal(result$submission$provisional_id, 12345)
  expect_equal(result$samples$alias, "Sample1")
  expect_equal(result$samples$provisional_id, 12345)
  expect_equal(result$studies$study, "Study1")
  expect_equal(result$studies$provisional_id, 12345)
  expect_equal(result$experiments$study, "Study1")
  expect_equal(result$experiments$experiment, "Experiment1")
  expect_equal(result$experiments$provisional_id, 12345)
  expect_equal(result$runs$run, "Run1")
  expect_equal(result$runs$files[[1]], c(raw.fastq.gz.c4gh = 12345))
  expect_equal(result$analyses$study_provisional_id, c(Study1 = 12345))
  expect_equal(result$analyses$files[[1]], c(processed.bam.c4gh = 12345))
  expect_equal(result$datasets$dataset, "Dataset1")
  expect_equal(result$datasets$run_provisional_ids[[1]], c(Run1 = 12345))
})

test_that("resumes submission correctly when an ID is provided", {
  mock_client <- list(
    get__samples = function() data.frame(alias = character()),
    get__files = function(prefix) data.frame(provisional_id = "f1"),
    get__submissions__provisional_id = function(id) list(provisional_id = id)
  )

  local_mocked_bindings(
    create_client = function(...) mock_client,
    get_or_post = function(id, data, client, type, ...) {
      data$provisional_id <- id
      data
    }
  )

  result <- new_submission(request_data = minimal_metadata, id = 56789)

  expect_equal(result$submission$provisional_id, 56789)
  expect_equal(result$studies$study, "Study1")
  expect_equal(result$studies$provisional_id, 56789)
  expect_equal(result$runs$run, "Run1")
  expect_equal(result$runs$files[[1]], c(raw.fastq.gz.c4gh = "f1"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("throws error if request_data is not a named list", {
  expect_error(new_submission(request_data = list(1, 2, 3)), "must be a named list")
  expect_error(new_submission(request_data = "not a list"), "must be a named list")
})

test_that("stops if sample aliases already exist in database and retrieve_if_exists is FALSE", {
  mock_client <- list(
    get__samples = function() data.frame(alias = "Sample1", stringsAsFactors = FALSE)
  )

  local_mocked_bindings(
    create_client = function(...) mock_client
  )

  expect_error(
    new_submission(request_data = minimal_metadata, mock_client, retrieve_if_exists = FALSE),
    "Samples aliases per submitter must be unique"
  )
})

test_that("stops if sample aliases already exist in database and retrieve_if_exists is FALSE", {
  mock_client <- list(
    get__samples = function() data.frame(alias = "Sample1", stringsAsFactors = FALSE)
  )

  local_mocked_bindings(
    create_client = function(...) mock_client
  )

  expect_error(
    new_submission(request_data = minimal_metadata, mock_client, retrieve_if_exists = FALSE),
    "Samples aliases per submitter must be unique"
  )
})

test_that("stops if files are missing from the EGA Inbox", {
  mock_client <- list(
    get__samples = function() data.frame(alias = character()),
    get__files = function(prefix) data.frame()
  )

  local_mocked_bindings(
    create_client = function(...) mock_client
  )

  expect_error(new_submission(request_data = mock_request_data))
})

test_that("gracefully handles non-logical retrieve_if_exists with a message", {
  mock_client <- list(
    get__samples = function() data.frame(alias = character()),
    get__files = function(prefix) data.frame(provisional_id = "f1"),
    post__submissions = function(body) list(provisional_id = "sub_1")
  )

  local_mocked_bindings(
    create_client = function(...) mock_client,
    get_or_post = function(id, data, client, type, ...) {
      data$provisional_id <- 12345
      data
    }
  )

  expect_message(
    new_submission(request_data = minimal_metadata, retrieve_if_exists = "not_logical"),
    "Setting to FALSE"
  )
})
