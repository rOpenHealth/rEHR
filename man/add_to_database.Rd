\name{add_to_database}
\alias{add_to_database}
\title{Adds a series of files to a database}
\usage{
  add_to_database(db, files, table_name,
    dateformat = "\%d/\%m/\%Y", yob_origin = 1800)
}
\arguments{
  \item{db}{a database connection object}

  \item{files}{a character vector of filenames to files to
  be imported}

  \item{table_name}{a name for the table to import to}

  \item{dateformat}{the format that dates are stored in the
  CPRD data.  If this is wrong it won't break but all dates
  are likely to be NA}

  \item{yob_origin}{value to add yob values to to get
  actual year of birth (Generally 1800)}
}
\description{
  This function can be used to import a CPRD file or files
  into a SQLite database connection.
}
\details{
  Will automatically unzip files before calling them in
}

