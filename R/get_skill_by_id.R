#' Retrieve Skill Information by ESCO Skill ID
#'
#' This function connects to the database and retrieves information from the
#' `adem.skills` table for a given ESCO Skill ID. The ID should be provided as a full URI string,
#' for example: `"http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02"`.
#'
#' @param skill_id A character string containing the full ESCO Skill URI.
#' For example: `"http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02"`.
#'
#' @return A data frame containing the information of the skill corresponding to
#' the specified `skill_id`. If the ID does not exist in the database, an empty data frame is returned.
#'
#' @export
#'
#' @examples
#' # Retrieve a skill using its ESCO Skill URI
#' get_skill_by_id("http://data.europa.eu/esco/skill/000f1d3d-220f-4789-9c0a-cc742521fb02")
#'
get_skill_by_id <- function(skill_id){
  con <- connect_db()
  output <- DBI::dbGetQuery(
    con,
    glue::glue_sql("
    SELECT *
    FROM adem.skills
    WHERE skill_id = {skill_id}
    ORDER BY skill_id ASC",
      .con = con
    ))
  dbDisconnect(con)
  return(output)
}
