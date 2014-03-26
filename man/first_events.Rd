\name{first_events}
\alias{first_events}
\title{Selects the earliest event grouped by patient}
\usage{
first_events(db = NULL, tab, columns = "eventdate", where = NULL,
  sql_only = FALSE, group_column = "patid", date_column = "eventdate")
}
\arguments{
  \item{db}{A database connection object}

  \item{tab}{the database table to extract from}

  \item{group_column}{column to group by.  Default is
  patid}

  \item{columns}{The other columns to be extracted}

  \item{where}{sting representation of the selection
  criteria}

  \item{date_column}{the column to sort by.  default is
  eventdate}

  \item{sql_only}{logical should the function just return a
  string of the SQL query?}
}
\value{
a dataframe or a string representing an sql query
}
\description{
This function runs a select_events() query and then groups
by patient id and picks only the earliest event for each
patient
}
\examples{
\dontrun{
b1 <- first_events(db, tab = "Clinical", columns = c("eventdate", "medcode"), where = "medcode \%in\% .(a$medcode)")
first_events(tab = "Clinical", columns = c("eventdate", "medcode"), where = "medcode \%in\% c(1, 2, 3, 4)", sql_only = TRUE)
}
}

