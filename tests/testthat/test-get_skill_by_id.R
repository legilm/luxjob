test_that("get_skill_by_id() returns all skills when no skill_id is provided", {
  result <- get_skill_by_id()
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > 0)
})

test_that("get_skill_by_id() returns a specific skill when skill_id is provided", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")

  skill_id <- "http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02"
  result <- get_skill_by_id(skill_id)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$skill_id, skill_id)
})

test_that("get_skill_by_id() returns NULL when skill_id does not exist", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")

  skill_id <- "http://data.europa.eu/esco/skill/nonexistent-skill-id"
  result <- get_skill_by_id(skill_id)
  expect_equal(result, "This skill name do not exist in our database")
})

test_that("get_skill_by_id() raises an error for invalid skill_id input", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")

  expect_error(get_skill_by_id(NA), "Error: The 'skill_id' argument must be a character string.")
})


test_that("get_skill_by_id() raises an error for invalid skill_id input", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")
  expect_error(get_skill_by_id(12345), "Error: The 'skill_id' argument must be a character string.")
})

test_that("get_skill_by_id() returns a data frame with the expected columns", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")

  result <- get_skill_by_id()
  expected_columns <- c("skill_id", "skill_label")
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("get_skills() returns a data frame with the expected column types", {
  skills <- get_skills()
  expect_type(skills$skill_id, "character")
  expect_type(skills$skill_label, "character")
})
