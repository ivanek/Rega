# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Row has non-NA chromosomes => returns a data.frame with id & label", {
  metadata <- list(
    analyses = data.frame(
      chromosomes = I(list(
        NA,
        list("group1--1--chr1--name1", "group2--3--chr3--name3")
      )),
      chromosome_groups = NA,
      stringsAsFactors = FALSE
    )
  )

  result <- format_chromosomes(metadata)

  expect_length(result, 2)
  expect_equal(result[[1]], list())
  expect_s3_class(result[[2]], "data.frame")
  expect_equal(result[[2]]$id, c(1, 3))
  expect_equal(result[[2]]$label, c("chr1", "chr3"))
})

test_that("Row has NA chromosomes but non-NA chromosome_groups", {
  metadata <- list(
    analyses = data.frame(
      chromosomes = I(list(
        NA,
        NA
      )),
      chromosome_groups = c("group1", NA),
      stringsAsFactors = FALSE
    ),
    select_input_data = list(
      chromosomes = c(
        "group1--1--chr1--name1",
        "group1--2--chr2--name2",
        "group2--3--chr1--name1"
      )
    )
  )

  result <- format_chromosomes(metadata)

  expect_length(result, 2)
  expect_s3_class(result[[1]], "data.frame")
  expect_equal(result[[1]]$id, c(1, 2))
  expect_equal(result[[1]]$label, c("chr1", "chr2"))

  expect_equal(result[[2]], list())
})

test_that("Row is missing 'chromosomes' column, but 'chromosome_groups' are present", {
  metadata <- list(
    analyses = data.frame(
      not_chromosomes = "val",
      chromosome_groups = c("group2"),
      stringsAsFactors = FALSE
    ),
    select_input_data = list(
      chromosomes = c(
        "group1--1--chr1--name1",
        "group1--2--chr2--name2",
        "group2--3--chr1--name1"
      )
    )
  )

  result <- format_chromosomes(metadata)

  expect_length(result, 1)
  expect_s3_class(result[[1]], "data.frame")
  expect_equal(result[[1]]$id, c(3))
  expect_equal(result[[1]]$label, c("chr1"))
})

test_that("Neither 'chromosomes' nor 'chromosome_groups' columns are present", {
  metadata <- list(
    analyses = data.frame(
      not_chromosomes = "val",
      stringsAsFactors = FALSE
    ),
    select_input_data = list(
      chromosomes = c(
        "group1--1--chr1--name1",
        "group1--2--chr2--name2",
        "group2--3--chr1--name1"
      )
    )
  )

  result <- format_chromosomes(metadata)

  expect_length(result, 1)
  expect_equal(result[[1]], list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'metadata$analyses' is not a data frame", {
  metadata <- list(
    analyses = c("invalid", "structure"),
    select_input_data = list(chromosomes = "dummy")
  )

  expect_error(
    format_chromosomes(metadata),
    "must have a positive length|must be a data frame"
  )
})


test_that("'select_input_data$chromosomes' missing or NULL", {
  metadata <- list(
    analyses = data.frame(
      chromosomes = NA,
      chromosome_groups = c("group1"),
      stringsAsFactors = FALSE
    ),
    select_input_data = list()
  )

  expect_error(format_chromosomes(metadata), "No chromosome data present")
})

test_that("malformed 'chromosomes' string that doesn't contain enough '--' parts", {
  metadata <- list(
    analyses = data.frame(
      chromosomes = c("chr1--a--b", "chr--b"),
      chromosome_groups = NA,
      stringsAsFactors = FALSE
    ),
    select_input_data = list(chromosomes = "dummy")
  )

  expect_error(format_chromosomes(metadata), "Malformed chromosome string")
})
