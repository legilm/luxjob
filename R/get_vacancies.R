#' Get Vacancies from ADEM Database
#'
#' Retrieves job vacancies from the ADEM database with optional filtering
#' by skill, company name, and canton. Returns a data frame with the selected
#' vacancy details.
#'
#' @param skill Character. Optional. Filter vacancies by required skill label (e.g., "German").
#' @param company Character. Optional. Filter vacancies by company name (e.g., "Company_2").
#' @param canton Character. Optional. Filter vacancies by canton (e.g., "Luxembourg").
#' @param limit Integer. Number of rows to return. Default is 100.
#'
#' @return A data frame containing the vacancies that match the filters, or `NULL` if no matches are found.
#'
#' @details
#' The function connects to the ADEM database, performs the query with any applicable filters,
#' and returns a data frame with columns such as `vacancy_id`, `company_id`, `occupation`,
#' `canton`, `year`, `month`, `skill_label`, and `company_name`. If all filter parameters are `NULL`,
#' it retrieves the latest `limit` vacancies without filtering.
#'
#' @export
#'
#' @examples
#' # Get 50 vacancies requiring Python in Luxembourg canton
#' get_vacancies(skill = "Python", canton = "Luxembourg", limit = 50)
#'
#' # Get all vacancies for the company "Amazon"
#' get_vacancies(company = "Amazon")
#'
#' # Get vacancies without any filters (default limit = 100)
#' get_vacancies()

get_vacancies <- function(skill = NULL, company = NULL, canton = NULL, limit = 100) {
  con <- connect_db()

  # Assemble the conditional parts
  filters <- c()

  if (!is.null(skill)) {
    filters <- c(filters, glue::glue_sql("s.skill_label = {skill}", .con = con))
  }
  if (!is.null(company)) {
    filters <- c(filters, glue::glue_sql("c.name = {company}", .con = con))
  }
  if (!is.null(canton)) {
    filters <- c(filters, glue::glue_sql("v.canton = {canton}", .con = con))
  }

  # Build the complete WHERE as a raw SQL string
  where_clause <- if (length(filters) > 0) {
    DBI::SQL(paste("WHERE", paste(filters, collapse = " AND ")))
  } else {
    DBI::SQL("")
  }

  # Mount the final query
  query <- glue::glue_sql("
    SELECT v.vacancy_id, v.company_id, v.occupation, v.canton, v.year, v.month,
           s.skill_label, c.name AS company_name
    FROM adem.vacancies v
    JOIN adem.companies c ON c.company_id = v.company_id
    JOIN adem.vacancy_skills vs ON vs.vacancy_id = v.vacancy_id
    JOIN adem.skills s ON s.skill_id = vs.skill_id
    {where_clause}
    LIMIT {limit}
  ", .con = con)

  output <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)

  if (nrow(output) == 0) {
    return(NULL)
  }

  return(output)
}
