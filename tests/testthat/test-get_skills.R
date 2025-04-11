test_that("get_skills() returns the expected number of records", {
  # Test the default behavior (100 records)
  skills <- get_skills()
  expect_equal(nrow(skills), 100)

  # Test with a custom limit
  skills_10 <- get_skills(10)
  expect_equal(nrow(skills_10), 10)
})

# Test valid cases
test_that("The 'limit' argument must be a non-null integer", {
  expect_no_error(get_skills(limit = 100))
})


  # Test invalid cases
test_that("The 'limit' argument must be a non-null integer", {
  expect_error({
    get_skills(limit = "a")
  }, "Error: The 'limit' argument must be a non-null integer.")

  expect_error({
    get_skills(limit = c(1, 2))
  }, "Error: The 'limit' argument must be a non-null integer.")

  expect_error({
    get_skills(limit = NA)
  }, "Error: The 'limit' argument must be a non-null integer.")

  expect_error({
    get_skills(limit = 1.5)
  }, "Error: The 'limit' argument must be a non-null integer.")
})


test_that("get_skills() returns a data frame with the expected columns", {
  skills <- get_skills()
  expected_columns <- c("skill_id", "skill_label")
  expect_true(all(expected_columns %in% colnames(skills)))
})

test_that("get_skills() returns a data frame with the expected column types", {
  skills <- get_skills()
  expect_type(skills$skill_id, "character")
  expect_type(skills$skill_label, "character")
})
