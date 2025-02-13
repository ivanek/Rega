# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Writes a list as YAML to a valid file", {
  responses <- list(a = 1, b = "two", c = list(sub = "nested"))
  tmp_file <- tempfile(fileext = ".yaml")

  expect_silent(save_log(responses, tmp_file))
  expect_true(file.exists(tmp_file))

  result <- yaml::read_yaml(tmp_file)
  expect_equal(result, responses)
})

test_that("Logfile = NULL does nothing, no error", {
  responses <- list(x = 1, y = 2)
  expect_silent(save_log(responses, logfile = NULL))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'logfile' is a directory", {
  dir_path <- tempdir()
  responses <- list(a = 1)
  expect_error(
    save_log(responses, logfile = dir_path),
    "Specified 'logfile' is a direcory."
  )
})

test_that("'logfile' path is invalid or unwritable", {
  responses <- list(a = 1)
  bad_path <- file.path("/", "root", "some_protected_place", "output.yaml")

  expect_error(
    suppressWarnings(save_log(responses, logfile = bad_path)),
    "cannot open the connection"
  )
})
