# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Some required fields, filter_ids=TRUE removes ID fields", {
  schema <- list(
    required = c("title", "description"),
    properties = list(
      title = list(type = "string"),
      description = list(type = "string"),
      accession_id = list(type = "string"),
      policy_accession_id = list(type = "string")
    )
  )
  result <- get_properties(schema, filter_ids = TRUE)

  expect_equal(
    result,
    c("* Title", "* Description", "Policy Accession Id")
  )
})

test_that("No required fields, filter_ids=FALSE => keeps ID fields", {
  schema <- list(
    required = character(0),
    properties = list(
      experiment_field = list(type = "string"),
      provisional_id = list(type = "string")
    )
  )
  result <- get_properties(schema, filter_ids = FALSE)

  expect_equal(result, c("Experiment Field", "Provisional Id"))
})

test_that("All fields are required, multiple underscores", {
  schema <- list(
    required = c("my_big_field", "another_field"),
    properties = list(
      my_big_field = list(type = "string"),
      another_field = list(type = "string")
    )
  )
  result <- get_properties(schema, filter_ids = TRUE)

  expect_equal(result, c("* My Big Field", "* Another Field"))
})

test_that("'schema$required' is missing or not a vector", {
  incomplete_schema <- list(properties = list(field1 = list(type = "string")))
  expect_equal(get_properties(incomplete_schema), "Field1")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'schema' is not a list => $ operator fails", {
  not_a_list <- "I am not a list"
  expect_error(get_properties(not_a_list), "'schema' must be a named list")
})

test_that("'schema' has no names", {
  l <- list("a", "b", "c")
  expect_error(
    get_properties(l), "'schema' must be a named list"
  )
})

test_that("'schema$properties' is missing", {
  bad_schema <- list(required = c("a", "b"))
  expect_error(get_properties(bad_schema), "No 'properties' key in schema")
})
