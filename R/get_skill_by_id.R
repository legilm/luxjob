#' Retrieve Skill Information from the Database
#'
#' This function retrieves skill information from the `adem.skills` table in the connected database.
#' The user can specify a `skill_id` to retrieve a specific skill or omit it to return all skills in the table.
#'
#' @param skill_id Character. Optional. The URI or identifier of a specific skill based on the ESCO classification (e.g., `"http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02"`).
#' If `NULL` or not provided, the function will retrieve all skills available in the `adem.skills` table.
#'
#' @return A data frame containing the skill information. If the `skill_id` is provided and does not exist in the database,
#' a character string is returned indicating that the skill does not exist.
#'
#' @details
#' The function opens a connection to the database using the internal helper function `connect_db()`,
#' executes an SQL query to fetch the data, and then closes the connection before returning the result.
#'
#' - If `skill_id` is provided, the function performs a filtered query by `skill_id`.
#' - If `skill_id` is `NULL`, it retrieves the entire `adem.skills` table.
#'
#' The function assumes that the `connect_db()` function and the `glue` and `DBI` packages are available.
#'
#' @export
#'
#' @examples
#' # Example 1: Retrieve a specific skill by ID
#' get_skill_by_id("http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02")
#'
#' # Example 2: Retrieve all skills
#' get_skill_by_id()
#'
#' # Example 3: Error when non-character input is provided
#' \dontrun{
#' get_skill_by_id(12345) # Will raise an error
#' }
#' @seealso [connect_db()] for the database connection function
#'
get_skill_by_id <- function(skill_id = NULL) {
  if (!is.null(skill_id) && !is.character(skill_id)) {
    stop("Error: The 'skill_id' argument must be a character string.")
  }

  con <- connect_db()
  if (!is.null(skill_id) && nchar(skill_id) > 0) {
    output <- DBI::dbGetQuery(
      con,
      glue::glue_sql("
      SELECT *
      FROM adem.skills
      WHERE skill_id = {skill_id}
      ", .con = con)
    )
  } else {
    output <- DBI::dbGetQuery(
      con,
      glue::glue_sql("
      SELECT *
      FROM adem.skills
                     "))
  }
  DBI::dbDisconnect(con)

  if (nrow(output) == 0) {
    return("This skill name do not exist in our database")
  }

  return(output)
}
