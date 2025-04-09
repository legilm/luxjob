#' Retrieve Skills from ADEM Database
#'
#' This function connects to the ADEM database and retrieves a list of skills
#' from the `skills` table, ordered by `skill_id`. The number of records
#' returned can be limited using the `limit` parameter.
#'
#' @param limit Integer. The maximum number of skill records to retrieve. Default is 100.
#'
#' @return A data frame containing skill records from the ADEM database.
#'
#' @details
#' The function uses a database connection established through `connect_db()` and performs
#' a SQL query using `glue_sql()` to safely inject the `limit` parameter. The connection
#' is closed automatically after the query is executed.
#'
#' @seealso [connect_db()], [DBI::dbGetQuery()], [glue::glue_sql()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve default 100 skill records
#' get_skills()
#'
#' # Retrieve only 10 skill records
#' get_skills(limit = 10)
#' }
#
get_skills <- function(limit = 100){
  con <- connect_db()
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT *
    FROM adem.skills
    ORDER BY skill_id ASC
    LIMIT {limit}",
    .con = con
    ))
  dbDisconnect(con)
  return(output)
}
