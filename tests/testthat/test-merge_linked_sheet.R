# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Merges a non-NA target with dat on 'source'", {
  target <- c("A", "B", "C")
  source <- "Key"
  dat <- data.frame(
    Key = c("A", "B", "C", "D"),
    Value = 1:4,
    stringsAsFactors = FALSE
  )
  sheet <- "not_collaborators"
  result <- merge_linked_sheet(target, source, dat, sheet)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result[[source]], c("A", "B", "C"))
  expect_equal(result$Value, 1:3)
})

test_that("Sheet is 'collaborators', modifies 'id' as integer", {
  target <- c("A", "B")
  source <- "Key"
  dat <- data.frame(
    Key = c("A", "B", "C"),
    id = c("10", "11", "12"),
    stringsAsFactors = FALSE
  )
  sheet <- "collaborators"
  result <- merge_linked_sheet(target, source, dat, sheet)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$Key, c("A", "B"))
  expect_type(result$id, "integer")
  expect_equal(result$id, c(10L, 11L))
})

test_that("'dat' is list", {
  # no extra information will get merged into target
  target <- c("A", "B")
  source <- "Key"
  dat_list <- list(Key = c("A", "B", "C"))
  sheet <- "any_sheet"
  result <- merge_linked_sheet(target, source, dat_list, sheet)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 1)
  expect_equal(result$Key, c("A", "B"))
})


test_that("No common values, empty merge result", {
  target <- c("D", "E")
  source <- "Key"
  sheet <- "any_sheet"
  dat <- data.frame("Key" = c(1, 2, 3))
  result <- merge_linked_sheet(target, source, dat, sheet)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 1)
  expect_true(names(result) == c("Key"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------
test_that("'source' column does not exist in 'dat'", {
  target <- c("A", "B")
  source <- "MissingKey"
  dat <- data.frame(Key = c("A", "B"), stringsAsFactors = FALSE)
  sheet <- "any_sheet"

  expect_error(merge_linked_sheet(target, source, dat, sheet))
})

test_that("'target' is incompatible (e.g., data frame instead of vector)", {
  target_df <- data.frame(A = 1:2, B = 3:4) # not a simple vector
  source <- "Key"
  dat <- data.frame(Key = 1:2, Value = c("X", "Y"), stringsAsFactors = FALSE)
  sheet <- "any_sheet"

  expect_error(merge_linked_sheet(target_df, source, dat, sheet))
})

test_that("sheet is 'collaborators' but 'id' is missing in merged data", {
  target <- c("D", "E") # won't match anything in 'dat'
  source <- "Key"
  # 'dat' has no 'id' column
  dat <- data.frame(Key = c("A", "B", "C"), stringsAsFactors = FALSE)
  sheet <- "collaborators"

  expect_error(merge_linked_sheet(target, source, dat, sheet))
})
