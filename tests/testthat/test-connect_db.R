test_that("connect_db() returns a PqConnection", {
  skip_if_not(Sys.getenv("PG_DB") != "", "Database env vars not set")

  con <- connect_db()
  expect_s4_class(con, "PqConnection")

  DBI::dbDisconnect(con)
})


test_that("connect_db() throws error if env vars are missing", {
  withr::with_envvar(c(PG_DB = NA, PG_HOST = NA, PG_USER = NA, PG_PASSWORD = NA), {
    expect_error(connect_db())
  })
})
