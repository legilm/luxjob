#' Retrieve Learning Tracks from ADEM Database
#'
#' This function connects to the ADEM database and retrieves information about learning tracks.
#' If a specific `track_id` is provided, it will return only the learning track that matches the given ID.
#' Otherwise, it returns all available learning tracks, ordered by `track_id`.
#'
#' @param track_id Integer or NULL. Optional. The ID of the learning track to retrieve.
#' If `NULL` (default), all learning tracks will be returned.
#'
#' @return A data frame containing one or more learning tracks with the following columns:
#' \describe{
#'   \item{track_id}{The ID of the learning track}
#'   \item{title}{The title of the learning track}
#'   \item{description}{A brief description of the learning track}
#'   \item{url}{A URL with more information about the learning track}
#' }
#' If no learning tracks are found, the function returns `NULL`.
#'
#' @export
#'
#' @examples
#' # Retrieve all learning tracks
#' get_learning_tracks()
#'
#' # Retrieve a specific learning track by ID
#' get_learning_tracks(track_id = 3)
get_learning_tracks <- function(track_id = NULL){

  con <- connect_db()
  # Empty filter variable
  filter <- glue::glue_sql("")
  # Retrieve Learning Tracks ids from ADEM Database
  if (!is.null(track_id)) {
    filter <- (glue::glue_sql("WHERE lt.track_id = {track_id}", .con = con))
  }
  # Mount the final query
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT lt.track_id, lt.title, lt.description, lt.url
    FROM adem.learning_tracks lt
    {filter}
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
