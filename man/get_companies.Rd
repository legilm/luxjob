% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_companies.R
\name{get_companies}
\alias{get_companies}
\title{Retrieve a List of Companies from the ADEM Database}
\usage{
get_companies(limit = 100)
}
\arguments{
\item{limit}{Integer. The maximum number of company records to retrieve. Defaults to 100.}
}
\value{
A data frame containing the retrieved company records, with each row representing a company
and columns corresponding to the fields in the \code{adem.companies} table.
}
\description{
This function connects to the ADEM database and retrieves a list of companies
ordered alphabetically by name. It allows limiting the number of records returned.
}
\details{
The function establishes a connection to the database using \code{connect_db()},
queries the \code{adem.companies} table, and returns the results as a data frame.
The SQL query is safely constructed using \code{glue::glue_sql()} to prevent SQL injection.

The connection is closed automatically after the query is executed.
}
\examples{
# Get the default number of companies (100)
get_companies()

# Get a custom number of companies (e.g., 50)
get_companies(limit = 50)

}
\seealso{
\code{\link[DBI:dbGetQuery]{DBI::dbGetQuery()}}, \code{\link[glue:glue_sql]{glue::glue_sql()}}, \code{\link[=connect_db]{connect_db()}}
}
