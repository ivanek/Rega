# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("'op' has a requestBody and a non-NULL schema", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list(
          schema = list(type = "object", properties = list(a = "string"))
        )
      )
    )
  )

  result <- Rega:::.add_json_validation(op)

  expect_length(result, 2)
  expect_true(is.call(result[[1]]) || is.expression(result[[1]]))
  expect_true(is.call(result[[2]]) || is.expression(result[[2]]))

  # Must deparse multiline expression, checking directly with bquote doesn't
  # work
  result1_text <- deparse(result[[1]])
  expect_true(any(grepl("validate_schema\\(body,", result1_text)))
  expect_true(any(grepl('a = \"string\"', result1_text)))

  result2_text <- deparse(result[[2]])
  expect_true(any(grepl("if \\(!valid\\)", result2_text)))
  expect_true(any(grepl("stop\\(validation_to_msg\\(valid\\)", result2_text)))
})

test_that("'op' has a requestBody and a complex schema", {
  # Taken from EGA API
  op <- list(requestBody = list(
    required = TRUE,
    content = list(`application/json` = list(schema = list(
      type = "object", properties = list(
        alias = list(
          type = "string",
          description = "The sample alias", example = "My Sample Alias"
        ),
        title = list(
          type = "string", description = "The sample title",
          example = "My Sample"
        ), description = list(
          type = "string", description = "The sample description",
          example = "My Sample Description"
        ), biological_sex = list(
          type = "string", description = "The sample biological sex. See enum [/enums/biological_sex](paths/enums-biological_sex/get)\n",
          example = "male"
        ), subject_id = list(
          type = "string",
          description = "The sample subject ID", example = "92837728-223872899"
        ),
        phenotype = list(
          type = "string", description = "The sample phenotype",
          example = "Cancer"
        ), biosample_id = list(
          type = "string",
          description = "The sample biosample ID", example = NULL
        ),
        case_control = list(
          type = "string", description = "The sample case control. See enum [/enums/case_controls](/paths/enums-case_controls/get)\n",
          example = "case"
        ), organism_part = list(
          type = "string",
          description = "The sample organism part", example = NULL
        ),
        cell_line = list(
          type = "string", description = "The sample cell line",
          example = NULL
        ), extra_attributes = list(
          type = "array",
          description = "The extra attributes", items = list(
            properties = list(
              tag = list(type = "string"),
              value = list(type = "string"), unit = list(
                type = "string"
              )
            ), required = c(
              "tag",
              "value"
            )
          ), example = NULL
        )
      ), required = c(
        "alias",
        "biological_sex", "subject_id", "phenotype"
      )
    )))
  ))

  result <- Rega:::.add_json_validation(op)

  expect_length(result, 2)
  expect_true(is.call(result[[1]]) || is.expression(result[[1]]))
  expect_true(is.call(result[[2]]) || is.expression(result[[2]]))

  # Must deparse multiline expression, checking directly with bquote doesn't
  # work
  result1_text <- deparse(result[[1]])
  expect_true(any(grepl("validate_schema\\(body,", result1_text)))
  # Non exhaustive checks
  expect_true(any(grepl('alias = list\\(type = \"string\"', result1_text)))
  expect_true(any(grepl('description = list\\(type = \"string\"', result1_text)))
  expect_true(any(grepl('phenotype = list\\(type = \"string\"', result1_text)))
  expect_true(any(grepl('biosample_id = list\\(type = \"string\"', result1_text)))
  expect_true(any(grepl('cell_line = list\\(type = \"string\"', result1_text)))
  expect_true(any(grepl('required = c\\(\"alias\", \"biological_sex\"', result1_text)))
  expect_true(any(grepl('\"subject_id\", \"phenotype\"', result1_text)))
})

test_that("'op$schema' is not a list", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list(
          schema = 1234
        )
      )
    )
  )

  result <- Rega:::.add_json_validation(op)

  expect_length(result, 2)
  expect_true(is.call(result[[1]]) || is.expression(result[[1]]))
  expect_true(is.call(result[[2]]) || is.expression(result[[2]]))

  expect_equal(bquote(valid <- validate_schema(body, 1234)), result[[1]])
})

test_that("'op' has no requestBody", {
  op <- list()
  result <- Rega:::.add_json_validation(op)
  expect_equal(result, list())
})

test_that("'op' has requestBody, but no schema", {
  op <- list(
    requestBody = list(
      content = list(
        `application/json` = list()
      )
    )
  )

  result <- Rega:::.add_json_validation(op)
  expect_equal(result, list())
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'op' is not a list", {
  op <- "invalid value"
  expect_error(
    Rega:::.add_json_validation(op),
    "\\$ operator is invalid for atomic vectors|object of type 'character' is not subsettable"
  )
})
