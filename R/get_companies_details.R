#' Retrieve Company Information and Vacancies
#'
#' This function connects to the `adem` database and retrieves detailed information
#' for a specified company. It returns both the company information and all associated
#' vacancies by performing a join between the `adem.companies` and `adem.vacancies` tables.
#' If the company is not found, the function returns `NULL`.
#'
#' @param company_id Integer. A unique identifier for the company whose details and vacancies
#'   are to be retrieved. Defaults to 1.
#'
#' @returns A data frame containing the company information (including the column
#'   `company_name`) along with all its vacancies. If the company is not found, the function
#'   returns `NULL`.
#'
#' @examples
#' \dontrun{
#'   # Retrieve info and vacancies for the company with company_id 1
#'   get_companies_datails(1)
#'
#'   # Attempt to retrieve info for a non-existing company; returns NULL
#'   get_companies_datails(company_id = 9999)
#' }
#'
#' @export
#' @seealso [connect_db()], [DBI::dbGetQuery()], [glue::glue_sql()]
#'
get_companies_datails <- function(company_id = 1){
  con <- connect_db()
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT v.*, c.name AS company_name
    FROM adem.vacancies v
    JOIN adem.companies c ON c.company_id = v.company_id
    WHERE c.company_id = {company_id}",
                   .con = con
    ))
  DBI::dbDisconnect(con)
  if(nrow(output) == 0) {
    return(NULL)
  }
  return(output)
}
