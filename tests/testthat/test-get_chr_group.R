# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single entry in chr_enum, correct sep, matching group_id", {
  input <- "GRP1--10--my_label--my_name"
  result <- get_chr_group("GRP1", input, "--")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 10L)
  expect_equal(result$label, "my_label")
})

test_that("Multiple entries in chr_enum, returns only rows matching group_id", {
  input <- c(
    "GRP1--10--Label1--Name1",
    "GRP2--98--Label2--Name2",
    "GRP2--99--Label3--Name3"
  )
  result <- get_chr_group("GRP2", input, sep = "--")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(98L, 99L))
  expect_equal(result$label, c("Label2", "Label3"))
})

test_that("No rows match the given group_id (returns empty)", {
  input <- c("GRP1--10--Label1--Name1", "GRP3--25--Label2--Name2")
  result <- get_chr_group("GRP2", input, "--")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("id", "label"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'chr_enum' is not a character vector", {
  not_char <- 12345
  expect_error(get_chr_group("GRP1", not_char, "--"))
})

test_that("Entries in chr_enum do not split into exactly 4 parts", {
  input <- "GRP1--10--LabelOnly"

  expect_error(
    get_chr_group("GRP1", input, "--"),
    "must be the same length as the vector"
  )
})

test_that("Invalid 'sep' leads to empty result", {
  input <- "GRP1--10--Label--Name"
  result <- get_chr_group("GRP1", input, sep = "")
  expect_equal(names(result), c("id", "label"))
  expect_equal(nrow(result), 0)
})
