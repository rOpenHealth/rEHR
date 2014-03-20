\name{head.SQLiteConnection}
\alias{head.SQLiteConnection}
\title{head for \code{SQLiteConnection} object}
\usage{
\method{head}{SQLiteConnection}(x, table = NULL, n = 6L, ...)
}
\arguments{
  \item{x}{A \code{SQLiteConnection} object}

  \item{table}{character specifying a table}

  \item{n}{integer: Number of rows to output}

  \item{...}{Additional arguments}
}
\description{
If just a database connection is selected, returns a
dataframe of table names If a table name is also supplied,
the first n rows from this table are output
}

