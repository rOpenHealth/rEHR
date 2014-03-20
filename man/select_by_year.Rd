\name{select_by_year}
\alias{select_by_year}
\title{Runs a series of selects over a year range and collects in a list of dataframes}
\usage{
select_by_year(dbname, tables, columns = "*", where, year_range,
  year_fn = qof_years, as_list = TRUE, cores = 1L)
}
\arguments{
  \item{dbname}{path to the database file}

  \item{tables}{character vector of table names}

  \item{columns}{character vector of columns to be selected
  from the tables}

  \item{where}{character string representation of the
  selection criteria}

  \item{year_range}{integer vector of years to be queried}

  \item{year_fn}{function that determines how year start
  and end dates are calculated}

  \item{as_list}{logical: Should the results be returned as
  a list of years? If not, the data is collapsed into a
  dataframe}

  \item{cores}{integer: The number of processor cores
  available.}
}
\description{
This function applies a database select over a range of
years and outputs as a list or a dataframe The function can
be parallelised using \code{multicore}.  If cores are set
to 1, lapply is used without calling in multicore.
}
\details{
Because the same database connection cannot be used across
threads, the input is a path to a database rather than a
database connection itself and a new connection is made
with every fork.

Year start and year end criteria can be added to the where
argument as 'STARTDATE' and 'ENDDATE'.  These will get
translated to the correct start and end dates specified by
year_fn
}
\examples{
\dontrun{
# Output from a single table
where_q <- "crd < STARTDATE & (is.null(tod) | tod > ENDDATE) & accept == 1"
ayears <- select_by_year(db, "Patient", columns = c("patid", "yob", "tod"), where = where_q, year_range = 2000:2003)
# Output from multiple tables
load("data/medical.RData")
a <- read.csv("data/chronic-renal-disease.csv")
a <- read_to_medcodes(a, medical, "code", lookup_readcodes= "readcode", lookup_medcodes="medcode", description = T)
where_q <- "eventdate >= STARTDATE & eventdate <= ENDDATE & medcode \%in\% .(a$medcode)"
byears <- byears <- select_by_year("~/rOpenHealth/CPRD_test/Coupland/Coupland", c("Clinical", "Referral"),
columns = c("patid", "eventdate", "medcode"),
where = where_q, year_range = 2000:2003, as_list = FALSE,
cores = 10)
}
}

