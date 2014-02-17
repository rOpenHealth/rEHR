\name{database}
\alias{database}
\title{Wrapper for dbconnect}
\usage{
  database(dbname)
}
\arguments{
  \item{dbname}{a name for the new database}
}
\description{
  Connects to a SQLite database or creates one if it does
  not already exist
}
\details{
  '.sqlite' file extension is automatically added and does
  not need to be specified in the dbname argument.
}

