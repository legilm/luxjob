#' Retrieve book recommendations from the ADEM database
#'
#' This function queries the ADEM database to retrieve recommended books, optionally
#' filtered by a specific skill ID. The books are returned in ascending order by book ID.
#'
#' @param skill_id An optional integer specifying the skill ID to filter book recommendations.
#'   If NULL (default), all books are returned.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item{book_id: }{Unique identifier for the book}
#'   \item{title: }{Title of the book}
#'   \item{author: }{Author of the book}
#'   \item{skill_id: }{Skill ID associated with the book recommendation}
#' }
#' Returns NULL if no books match the query.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all books
#' all_books <- get_books()
#'
#' # Get books for a specific skill
#' programming_books <- get_books(skill_id = http://data.europa.eu/esco/skill/70198e4e-86ad-4acc-a9eb-e24e2c107d18)
#' }
#'
#' @seealso \code{\link{connect_db}} for the database connection function
#' @family ADEM database functions
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
get_books <- function(skill_id = NULL){
  # Check if it is a positive integer
  if (!is.numeric(company_id) || length(company_id) != 1 || is.na(company_id) || company_id %% 1 != 0) {
    stop("Error: The 'skill_id' argument must be a non-null integer.")
  }
  #connect to DB before the query
  con <- connect_db()
  # Empty filter variable
  filter <- glue::glue_sql("")
  # Retrieve Learning Tracks ids from ADEM Database
  if (!is.null(skill_id)) {
    filter <- (glue::glue_sql("WHERE br.skill_id = {skill_id}", .con = con))
  }
  # Mount the final query
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT br.book_id, br.title, br.author, br.skill_id
    FROM adem.book_recommendations br
    {filter}
    ORDER BY br.book_id ASC
	  ", .con = con
    ))
  DBI::dbDisconnect(con)

  # Check if the output is empty
  if(nrow(output) == 0) {
    return("This skill id number do not exist in our database")
  }
  return(output)
}

#'
