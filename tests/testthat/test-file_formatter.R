# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("file_formatter handles standard formatting and path logic", {
  dat <- data.frame(
    V1 = c("* File", "  file1.txt", "file2.txt  "),
    V2 = c("EGA Inbox Relative Path", "", "folder/sub"),
    stringsAsFactors = FALSE
  )

  params <- list(
    crypt_ext = "crypt4gh",
    prefix = "prefix_",
    prepend_slash = FALSE
  )

  result <- file_formatter(dat, params)

  expect_s3_class(result, "data.frame")
  expect_equal(
    result$ega_file,
    c("prefix_file1.txt.crypt4gh", "folder/sub/prefix_file2.txt.crypt4gh")
  )
  expect_equal(nrow(result), 2)
})

test_that("file_formatter handles standard formatting and path logic, alternative params", {
  dat <- data.frame(
    V1 = c("* File", "  file1.txt", "file2.txt  "),
    V2 = c("EGA Inbox Relative Path", "", "/folder/sub"),
    stringsAsFactors = FALSE
  )

  params <- list(
    crypt_ext = "",
    prefix = "",
    prepend_slash = TRUE
  )

  result <- file_formatter(dat, params)

  expect_s3_class(result, "data.frame")
  expect_equal(
    result$ega_file,
    c("/file1.txt", "/folder/sub/file2.txt")
  )
  expect_equal(nrow(result), 2)
})

test_that("file_formatter handles NULL params and empty paths", {
  dat <- data.frame(
    V1 = c("* File", "test.dat"),
    V2 = c("EGA Inbox Relative Path", NA)
  )
  result <- file_formatter(dat, NULL)

  expect_equal(result$ega_file[1], "test.dat")
})

test_that("file_formatter correctly strips initial slashes from paths", {
  dat <- data.frame(
    V1 = c("* File", "/f.txt"),
    V2 = c("EGA Inbox Relative Path", "/foo/bar")
  )

  result <- file_formatter(dat, list(prepend_slash = FALSE))
  expect_equal(result$ega_file[1], "foo/bar//f.txt")
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("file_formatter catches invalid input types", {
  expect_error(file_formatter(list(a = 1), list()), "'tab' must be a data frame")

  expect_error(
    file_formatter(data.frame(a = 1), params = "invalid"),
    "'params' must be a list, FALSE or NULL"
  )

  expect_error(
    file_formatter(data.frame(a = 1), params = c(1)),
    "'params' must be a list, FALSE or NULL"
  )

  expect_error(
    file_formatter(data.frame(a = 1), params = factor(TRUE)),
    "'params' must be a list, FALSE or NULL"
  )
})
