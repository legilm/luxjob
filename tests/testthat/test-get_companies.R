# tests/testthat/test-get_companies.R

test_that("get_companies returns a data frame", {
  result <- get_companies()
  expect_s3_class(result, "data.frame")
})

test_that("get_companies returns the correct number of rows", {
  limit <- 10
  result <- get_companies(limit = limit)
  expect_equal(nrow(result), limit)
})

test_that("get_companies returns columns expected from adem.companies", {
  result <- get_companies(limit = 1)
  expected_columns <- c("company_id", "name", "sector")
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("get_companies returns ordered data", {
  result <- get_companies(limit = 100)
  expect_equal(result$name, sort(result$name))
})

test_that("get_companies handles default limit properly", {
  result_default <- get_companies()
  expect_equal(nrow(result_default), 100)
})

