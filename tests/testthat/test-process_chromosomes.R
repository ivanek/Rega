# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------
test_that("Chromosomes specified, input lookup not used", {
  chr_data <- list(
    chromosomes = c("group1--1--chrA", "group1--2--chrB"),
    chromosome_groups = NA
  )
  select_input_data <- list()

  result <- process_chromosomes(chr_data, select_input_data)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("id", "label"))
  expect_equal(result$id, c(1, 2))
  expect_equal(result$label, c("chrA", "chrB"))
})

test_that("No chromosomes, but chromosome groups present", {
  chr_data <- list(
    chromosome_groups = c("group1")
  )
  select_input_data <- list(chromosomes = c(
    "group1--1--chr1--name1", "group1--2--chr2--name2", "group2--3--chrC--name3"
  ))

  result <- process_chromosomes(chr_data, select_input_data)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("id", "label"))
  expect_equal(result$id, c(1, 2))
  expect_equal(result$label, c("chr1", "chr2"))
})

test_that("No chromosomes and no chromosome groups present", {
  chr_data <- list(
    chromosomes = NA,
    chromosome_groups = NA
  )
  select_input_data <- list(chromosomes = c())

  result <- process_chromosomes(chr_data, select_input_data)
  expect_equal(result, list())
})

test_that("'chr_data' is data frame", {
  chr_data <- data.frame(
    chromosomes = c("sample--1--chrA--name2", "sample--2--chrB--name3")
  )
  result <- process_chromosomes(chr_data, list())

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("id", "label"))
  expect_equal(result$id, c(1, 2))
  expect_equal(result$label, c("chrA", "chrB"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Malformed chromosome string", {
  chr_data <- data.frame(chromosomes = c("sample--1"))
  expect_error(
    process_chromosomes(chr_data, NULL),
    "Malformed chromosome string."
  )
})

test_that("Chromosome groups present but no lookup data", {
  chr_data <- data.frame(chromosome_groups = c("group1"))
  expect_error(
    process_chromosomes(chr_data, list()),
    "No chromosome data present to match the groups."
  )
})

test_that("Incorrect column types", {
  chr_data <- data.frame(chromosomes = list(c("sample--1--chrA--name1")))
  expect_equal(process_chromosomes(chr_data, list()), list())
})

test_that("Empty chromosome strings", {
  chr_data <- list(chromosomes = c(NA, "", " "))
  expect_error(
    process_chromosomes(chr_data, list()),
    "Malformed chromosome string."
  )
})

test_that("Too many chromosome groups", {
  select_input_data <- list(chromosomes = c(
    "group1--1--chr1--name1", "group1--2--chr2--name2", "group2--3--chrC--name3"
  ))
  chr_data <- list(
    chromosomes = NA,
    chromosome_groups = c("group1", "group2")
  )
  expect_error(
    process_chromosomes(chr_data, select_input_data),
    "Too many chromosome groups specified"
  )
})
