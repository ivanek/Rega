# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that(".studies_extra_validator handles valid title and description and null aliases", {
  meta <- list(
    studies = data.frame(
      title = "Clinical Research Project with RNASeq analysis",
      description = paste0(
        "Lorem ipsum dolor sit amet. Consectetur adipiscing eli?",
        "Duis eu accumsan lorem! porta tristique lacus"
      )
    )
  )

  aliases <- list(id = "alias_1")

  result <- .studies_extra_validator(meta, aliases)
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(1, 4))
  expect_equal(result_summary$passes, rep(1, 4))
  expect_false(all(result_summary$error))

  result <- .studies_extra_validator(meta, NULL)
  result_summary <- summary(result)
  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(1, 4))
  expect_equal(result_summary$passes, rep(1, 4))
})


test_that(".studies_extra_validator catches logic failures in validation", {
  meta <- list(
    studies = data.frame(
      title = c(
        "Stu",
        "Studies for this submission",
        "Stu"
      ),
      description = c(
        "",
        "Cras ante lectus. tincidunt at tempus eget!",
        ""
      )
    )
  )

  result <- .studies_extra_validator(meta, list())
  result_summary <- summary(result)

  expect_s4_class(result, "validation")
  expect_equal(result_summary$items, rep(3, 4))
  expect_equal(result_summary$passes, c(1, 1, 1, 0))
  expect_equal(result_summary$fails, c(2, 2, 2, 3))
  expect_false(all(result_summary$error))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that(".studies_extra_validator catches errors in validation", {
  meta <- list(
    studies = data.frame(
      title = list("aaa"),
      description = paste0(
        "Lorem ipsum dolor sit amet. Consectetur adipiscing eli?"
      )
    )
  )

  result <- .studies_extra_validator(meta, list())
  result_summary <- summary(result)

  expect_equal(result_summary$items, c(0, 1, 0, 1))
  expect_equal(result_summary$fails, c(0, 0, 0, 1))
  expect_equal(result_summary$error, c(TRUE, FALSE, TRUE, FALSE))
})

test_that(".studies_extra_validator rejects invalid input types", {
  expect_error(
    .studies_extra_validator(meta = "not a list", aliases = list()),
    "'meta' must be a list"
  )
})

test_that("studies must be a data.frame", {
  meta <- list(studies = list(title = "Clinical Trial"))

  expect_error(
    .studies_extra_validator(meta, NULL),
    "unable to find an inherited method"
  )
})
