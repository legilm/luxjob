#' Get Vacancy Information by ID
#'
#' This function retrieves detailed vacancy information, including the related company
#' and skills required for a specific vacancy, identified by its `vacancy_id`. It constructs
#' and executes a SQL query to fetch the relevant data from the `adem.vacancies`, `adem.companies`,
#' `adem.vacancy_skills`, and `adem.skills` tables.
#'
#' @param vacancy_id A numeric or character value representing the unique identifier of the vacancy.
#'                   If `NULL`, all vacancies are returned (default is `NULL`).
#'
#' @return A data frame containing the vacancy details, including:
#' \itemize{
#'   \item vacancy_id
#'   \item vacancy_title
#'   \item company_name
#'   \item skill_label
#' }
#' If no vacancy is found for the provided `vacancy_id`, `NULL` is returned.
#'
#' @import DBI
#' @importFrom DBI dbDisconnect
#' @import glue
#' @examples
#' # Example of calling the function with a specific vacancy ID
#' result <- get_vacancy_by_id(vacancy_id = 123)
#'
#' # Example of calling the function without any specific ID, which retrieves all vacancies
#' all_vacancies <- get_vacancy_by_id()
#'
#' @export
get_vacancy_by_id <- function(vacancy_id = NULL){
  # Check if it is a positive integer
  if (!is.numeric(company_id) || length(company_id) != 1 || is.na(company_id) || company_id %% 1 != 0) {
    stop("Error: The 'company_id' argument must be a non-null integer.")
  }
  #connect to DB before the query
  con <- connect_db()
  # Empty filter variable
  filter <- glue::glue_sql("")
  # Retrieve vacancy ids from ADEM Database
  if (!is.null(vacancy_id)) {
    filter <- (glue::glue_sql(" v.vacancy_id = {vacancy_id}", .con = con))
  }
  # Build the complete WHERE as a raw SQL string
  where_clause <- if (!is.null(vacancy_id)) {
    DBI::SQL(paste("WHERE", filter))
  } else {
    DBI::SQL("")
  }
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT v.*, s.skill_label, c.name AS company_name
    FROM adem.vacancies v
    JOIN adem.companies c ON c.company_id = v.company_id
	  JOIN adem.vacancy_skills vs ON vs.vacancy_id = v.vacancy_id
	  JOIN adem.skills s ON s.skill_id = vs.skill_id
	  {where_clause}",
                   .con = con
    ))
  DBI::dbDisconnect(con)
  if(nrow(output) == 0) {
    return(NULL)
  }
  return(output)
}
