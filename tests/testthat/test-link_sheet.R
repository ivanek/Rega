# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("No sheets link to 'sheet1'", {
  df1 <- data.frame(id = 1:3, val = letters[1:3])
  df2 <- data.frame(other_data = c("X", "Y"))

  metadata <- list(
    sheet1 = df1,
    sheet2 = df2
  )

  result <- link_sheet(metadata, "sheet1")

  expect_equal(result, metadata)
})

test_that("One sheet references 'sheet1', merges data", {
  df1 <- data.frame(id = 1:3, val = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(
    something_else = c("A", "B", "C"),
    sheet1 = I(list(c(1, 999), c(2), c(1, 3)))
  )

  metadata <- list(
    sheet1 = df1,
    sheet2 = df2
  )

  result <- link_sheet(metadata, "sheet1")

  expect_true(is.list(result))
  expect_true("sheet2" %in% names(result))
  expect_true("sheet1" %in% names(result$sheet2))

  merged_list <- result$sheet2$sheet1
  expect_equal(length(result$sheet2$sheet1), 3)
  expect_s3_class(merged_list[[1]], "data.frame")
  expect_s3_class(merged_list[[2]], "data.frame")
  expect_s3_class(merged_list[[3]], "data.frame")

  expect_equal(nrow(merged_list[[1]]), 1)
  expect_equal(nrow(merged_list[[2]]), 1)
  expect_equal(nrow(merged_list[[3]]), 2)

  expect_equal(merged_list[[3]]$val, c("a", "c"))
})

test_that("Two sheets references, merges data", {
  # 'sheet1' is our source data
  df1 <- data.frame(id = 1:3, val = letters[1:3], stringsAsFactors = FALSE)

  df2 <- data.frame(
    name = c("X", "Y", "Z"),
    attribute = letters[1:3],
    stringsAsFactors = FALSE
  )

  df3 <- data.frame(
    something_else = c("A", "B"),
    sheet1 = I(list(c(1, 999), c(2))),
    sheet2 = I(list(c("Z"), c("X", "Y")))
  )

  metadata <- list(
    sheet1 = df1,
    sheet2 = df2,
    sheet3 = df3
  )

  result <- link_sheet(link_sheet(metadata, "sheet1"), "sheet2")

  expect_true(is.list(result))
  expect_true("sheet3" %in% names(result))
  expect_true(all(c("sheet1", "sheet2") %in% names(result$sheet3)))

  merged_list_1 <- result$sheet3$sheet1
  expect_equal(length(merged_list_1), 2)
  expect_equal(dim(merged_list_1[[1]]), c(1, 2))
  expect_equal(dim(merged_list_1[[2]]), c(1, 2))

  merged_list_2 <- result$sheet3$sheet2
  expect_equal(length(merged_list_2), 2)
  expect_equal(dim(merged_list_2[[1]]), c(1, 2))
  expect_equal(dim(merged_list_2[[2]]), c(2, 2))
})

test_that("Referencing sheet has no 'sheet_name' column to merge", {
  df1 <- data.frame(id = 1:2)
  df2 <- data.frame(something_else = c("A", "B")) # no 'sheet1' column

  metadata <- list(
    sheet1 = df1,
    sheet2 = df2
  )

  # Nothing should be done
  expect_equal(link_sheet(metadata, "sheet1"), metadata)
})


# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'sheet_name' does not exist in metadata", {
  metadata <- list(
    sheetA = data.frame(x = 1:3),
    sheetB = data.frame(y = 4:6)
  )

  expect_error(link_sheet(metadata, "sheetX"))
})

test_that("'source_data' has 0 rows but at least one linked sheet => triggers stop", {
  df1 <- data.frame(id = integer(0), val = character(0))
  # 'sheet2' references 'sheet1' with non-NA data => are_linked is TRUE
  df2 <- data.frame(
    sheet1 = I(list(c(1), c(2)))
  )

  metadata <- list(
    sheet1 = df1,
    sheet2 = df2
  )

  # Because nrow(df1) = 0 but are_linked = TRUE, we expect an error
  expect_error(
    link_sheet(metadata, "sheet1"),
    "no entries were found",
    fixed = TRUE
  )
})
