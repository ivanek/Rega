# ------------------------------------------------------------------------------
# 1) Happy Path Tests
# ------------------------------------------------------------------------------

test_that("get_entry_by_title happy paths", {
  mock_resp_studies <- data.frame(
    title = c("Study Alpha", "Study Beta", "Study GammaAlpha"),
    id = 1:3,
    stringsAsFactors = FALSE
  )

  mock_resp_samples <- data.frame(
    title = c("Sample Foo", "Sample Bar", "Sample Baz", "Sample GammaBeta"),
    id = 6:9,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    create_client = function(...) list(
      get__submissions = function() data.frame(),
      get__studies = function() mock_resp_studies,
      get__samples = function() mock_resp_samples,
      get__experiments = function() mock_resp_samples,
      get__runs = function() data.frame(),
      get__analyses = function() data.frame(),
      get__datasets = function() data.frame()
    )
  )

  # 1. Single Type, NULL client
  res <- get_entry_by_title("Alpha", type = "studies", client = NULL)
  expect_type(res, "list")
  expect_s3_class(res$studies, "data.frame")
  expect_equal(nrow(res$studies), 2)
  expect_equal(res$studies$title, c("Study Alpha", "Study GammaAlpha"))

  ## retrieve all types, custom client
  res_all <- get_entry_by_title("Alpha", type = c(), client = create_client())
  expect_length(res_all, 7)
  expect_equal(unname(sapply(res_all, nrow)), c(0, 2, 0, 0, 0, 0, 0))
  expect_equal(unname(sapply(res_all, ncol)), c(0, 2, 2, 2, 0, 0, 0))

  # 2. Multiple Types
  res_multi <- get_entry_by_title(
    "Beta", type = c("studies", "samples"), client = create_client()
  )
  expect_length(res_multi, 2)
  expect_named(res_multi, c("studies", "samples"))
  expect_equal(nrow(res_multi$studies), 1)
  expect_equal(nrow(res_multi$samples), 1)

  # 3. No Match
  res_empty <- get_entry_by_title(
    "NonExistentTitle", type = "studies", client = create_client()
  )
  expect_equal(nrow(res_empty$studies), 0)
  expect_equal(names(res_empty$studies), names(mock_resp_studies))
})

# ------------------------------------------------------------------------------
# 2) Error Path Tests
# ------------------------------------------------------------------------------

test_that("get_entry_by_title error paths", {
  # 1. Invalid Type
  expect_error(
    get_entry_by_title("Title", type = "wrong_type"),
    "Invalid types specified"
  )

  expect_error(
    get_entry_by_title("Title", type = 12345),
    "Invalid types specified"
  )

  # 2. Invalid Title
  expect_error(
    get_entry_by_title(c("Title1", "Title2")),
    "must be a non-empty character scalar"
  )

  expect_error(
    get_entry_by_title(NULL),
    "must be a non-empty character scalar"
  )

  expect_error(
    get_entry_by_title(list()),
    "must be a non-empty character scalar"
  )

  expect_error(
    get_entry_by_title(1),
    "must be a non-empty character scalar"
  )

  # 3. Client Validation
  expect_error(
    get_entry_by_title("Title", type = "studies", client = list()),
    "must be a non-empty list"
  )

  expect_error(
    get_entry_by_title("Title", type = "studies", client = "aaaa"),
    "must be a non-empty list"
  )

  expect_error(
    get_entry_by_title("Title", type = "studies", client = 123456),
    "must be a non-empty list"
  )
})
