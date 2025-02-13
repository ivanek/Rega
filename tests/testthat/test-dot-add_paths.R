# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("Single path param", {
  params <- c("id")

  result <- Rega:::.add_paths(params)

  expect_length(result, 1)
  expect_true(is.expression(result[[1]]) || is.call(result[[1]]))
  expect_equal(
    result[[1]],
    bquote(url <- sub("{id}", as.character(id), url, fixed = TRUE))
  )

  id = 15
  url = "www.example.com/{id}"
  eval(result[[1]])
  expect_equal("www.example.com/15", url)
})

test_that("Multiple path params => a list of multiple expressions", {
  params <- c("user", "repo")
  result <- Rega:::.add_paths(params)

  expect_length(result, 2)
  for (expr in result) {
    expect_true(is.expression(expr) || is.call(expr))
  }
  expect_equal(
    result[[1]],
    bquote(url <- sub("{user}", as.character(user), url, fixed = TRUE))
  )
  expect_equal(
    result[[2]],
    bquote(url <- sub("{repo}", as.character(repo), url, fixed = TRUE))
  )

  user = "Alice"
  repo = "github"
  url = "www.example.com/{user}/{repo}"
  eval(result[[1]])
  eval(result[[2]])
  expect_equal("www.example.com/Alice/github", url)

})

test_that("No path params", {
  params <- character(0)
  result <- Rega:::.add_paths(params)
  expect_equal(result, list())
})

test_that("No path params, empty list", {
  params <- list()
  result <- Rega:::.add_paths(params)
  expect_equal(result, list())
})

test_that("Element of 'path_params' is malformed, 'path_params' is a character vector", {
  params <- c("id", 123)
  result <- Rega:::.add_paths(params)

  expect_length(result, 2)
  for (expr in result) {
    expect_true(is.expression(expr) || is.call(expr))
  }
  expect_equal(
    result[[1]],
    bquote(url <- sub("{id}", as.character(id), url, fixed = TRUE))
  )
  expect_equal(
    result[[2]],
    bquote(url <- sub("{123}", as.character(`123`), url, fixed = TRUE))
  )
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("Element of 'path_params' is malformed, 'path_params' is a list", {
  params <- list("id", 123)
  expect_error(
    Rega:::.add_paths(params),
    "All 'path_params' must be character"
  )
})
