\name{import_CPRD_data}
\alias{import_CPRD_data}
\title{Imports all selected CPRD data into an sqlite database
Note that if you chose to import all the filetype, you may end up with aa very large database file.
You may then chose only to import the files you want to use.  You can always import the rest of the files later.}
\usage{
  import_CPRD_data(db, data_dir,
    filetypes = c("Additional", "Clinical", "Consultation", "Immunisation", "Patient", "Practice", "Referral", "Staff", "Test", "Therapy"),
    dateformat = "\%d/\%m/\%Y", yob_origin = 1800)
}
\arguments{
  \item{db}{a database connection}

  \item{data_dir}{the directory containing the CPRD cohort
  data}

  \item{filetypes}{character vector of filetypes to be
  imported}

  \item{dateformat}{the format that dates are stored in the
  CPRD data.  If this is wrong it won't break but all dates
  are likely to be NA}

  \item{yob_origin}{value to add yob values to to get
  actual year of birth (Generally 1800)}
}
\description{
  Imports all selected CPRD data into an sqlite database
  Note that if you chose to import all the filetype, you
  may end up with aa very large database file. You may then
  chose only to import the files you want to use.  You can
  always import the rest of the files later.
}

