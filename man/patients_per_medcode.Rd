\name{patients_per_medcode}
\alias{patients_per_medcode}
\title{Produce a dataset of CPRD medcodes with frequencies of patients in the clinical table
Note that this does not translate to Read/OXMIS codes}
\usage{
  patients_per_medcode(db, clinical_table = "Clinical",
    patid = "patid", medcode = "medcode")
}
\arguments{
  \item{db}{a database connection}

  \item{clinical_table}{name of the clinical table in the
  database}

  \item{patid}{name of the patid field}

  \item{medcode}{name of the medcode field}
}
\description{
  Produce a dataset of CPRD medcodes with frequencies of
  patients in the clinical table Note that this does not
  translate to Read/OXMIS codes
}
\examples{
\dontrun{
medcode_counts <- patients_per_medcode(db)
head(medcode_counts)
}
}

