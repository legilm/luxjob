library(testthat)

test_that("get_vacancies returns a data frame with expected columns", {

  result <- get_vacancies(limit = 5)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("vacancy_id", "company_id", "occupation", "canton",
                    "year", "month", "skill_label", "company_name") %in% names(result)))
})

test_that("get_vacancies returns NULL if no match is found", {

  result <- get_vacancies(skill = "NonexistentSkill_XYZ", limit = 10)
  expect_null(result)
})

test_that("get_vacancies works with only skill filter", {

  result <- get_vacancies(skill = "Python", limit = 10)
  if (!is.null(result)) {
    expect_true("Python" %in% result$skill_label)
  } else {
    succeed("No data found, but function returned NULL as expected.")
  }
})

test_that("get_vacancies works with multiple filters", {

  result <- get_vacancies(skill = "Python", company = "Amazon", canton = "Luxembourg", limit = 10)
  if (!is.null(result)) {
    expect_true(all(result$skill_label == "Python"))
    expect_true(all(result$company_name == "Amazon"))
    expect_true(all(result$canton == "Luxembourg"))
  } else {
    succeed("No data found, but function returned NULL as expected.")
  }
})
