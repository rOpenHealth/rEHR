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
Connects to a SQLite database or creates one if it does not
already exist
}
\details{
If the '.sqlite' file extension is ommited from the dbname
argument it is automatically added.
}

