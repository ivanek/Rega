# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Retrieves an enumeration from client with default prefix", {
  mock_client <- list(
    get__enums_colors = function() c("red", "blue", "green")
  )

  result <- get_enum(mock_client, "colors")
  expect_equal(result, c("red", "blue", "green"))
})

test_that("Retrieves an enumeration with a custom enum_prefix", {
  mock_client <- list(
    my_customprefix_shapes = function() c("circle", "square", "triangle")
  )

  result <- get_enum(mock_client, "shapes", enum_prefix = "my_customprefix_")
  expect_equal(result, c("circle", "square", "triangle"))
})

test_that("The enumeration function returns a data frame", {
  mock_client <- list(
    get__enums_people = function() data.frame(name = c("Alice", "Bob"))
  )

  result <- get_enum(mock_client, "people")
  expect_s3_class(result, "data.frame")
  expect_equal(result$name, c("Alice", "Bob"))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("'client' is not a list => subscript or object extraction fails", {
  client <- 123
  expect_error(
    get_enum(client, "colors"),
    "subscript out of bounds|attempt to apply subscript"
  )
})

test_that("Requested enum function doesn't exist in 'client'", {
  mock_client <- list(
    get__enums_shapes = function() c("square", "triangle")
  )

  expect_error(
    get_enum(mock_client, "colors"),
    "not found in client"
  )
})
