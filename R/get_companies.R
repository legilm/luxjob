#' Retrieve a List of Companies from the ADEM Database
#'
#' This function connects to the ADEM database and retrieves a list of companies
#' ordered alphabetically by name. It allows limiting the number of records returned.
#'
#' @param limit Integer. The maximum number of company records to retrieve. Defaults to 100.
#'
#' @return A data frame containing the retrieved company records, with each row representing a company
#' and columns corresponding to the fields in the `adem.companies` table.
#'
#' @details
#' The function establishes a connection to the database using `connect_db()`,
#' queries the `adem.companies` table, and returns the results as a data frame.
#' The SQL query is safely constructed using `glue::glue_sql()` to prevent SQL injection.
#'
#' The connection is closed automatically after the query is executed.
#'
#' @seealso [DBI::dbGetQuery()], [glue::glue_sql()], [connect_db()]
#'
#' @examples
#' # Get the default number of companies (100)
#' get_companies()
#'
#' # Get a custom number of companies (e.g., 50)
#' get_companies(limit = 50)
#'
#' @export

get_companies <- function(limit = 100){
  con <- connect_db()
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT *
    FROM adem.companies
    ORDER BY name ASC
    LIMIT {limit}",
                   .con = con
    ))
  DBI::dbDisconnect(con)
  return(output)
}
