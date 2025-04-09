#' Retrieve Learning Tracks from ADEM Database
#'
#' This function queries the ADEM database to retrieve information about learning tracks,
#' optionally filtered by a specific skill ID. It returns details such as track ID,
#' title, description, and URL for each matching learning track.
#'
#' @param skill_id An optional numeric or character value specifying the track ID to filter by.
#'   If NULL (default), all learning tracks are returned.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{track_id}{The unique identifier for the learning track}
#'   \item{title}{The title of the learning track}
#'   \item{description}{A description of the learning track}
#'   \item{url}{A URL linking to more information about the learning track}
#' }
#' Returns NULL if no matching records are found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all learning tracks
#' all_tracks <- get_learning_tracks()
#'
#' # Get a specific learning track by ID
#' specific_track <- get_learning_tracks(skill_id = 123)
#' }
#'
#' @seealso \code{\link{DBI::dbGetQuery}} for the database query function used internally
#' @family ADEM database functions
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @importFrom DBI dbDisconnect
#' @importFrom DBI SQL
get_learning_tracks <- function(skill_id = NULL){

  con <- connect_db()
  # Retrieve Learning Tracks ids from ADEM Database
  if (!is.null(skill_id)) {
    filter <- (glue::glue_sql("WHERE lt.track_id = {skill_id}", .con = con))
  }
  # Mount the final query
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT lt.track_id, lt.title, lt.description, lt.url
    FROM adem.learning_tracks lt
    {where_clause}
    ORDER BY track_id ASC
	  ",
                   .con = con
    ))
  DBI::dbDisconnect(con)

  # Check if the output is empty
  if(nrow(output) == 0) {
    return(NULL)
  }
  return(output)
}
