\name{import_CPRD_data}
\alias{import_CPRD_data}
\title{Imports all selected CPRD data into an sqlite database
Note that if you chose to import all the filetype, you may end up with aa very large database file.
You may then chose only to import the files you want to use.  You can always import the rest of the files later.}
\usage{
  import_CPRD_data(db, data_dir,
    filetypes = c("Additional", "Clinical", "Consultation", "Immunisation", "Patient", "Practice", "Referral", "Staff", "Test", "Therapy"))
}
\arguments{
  \item{db}{a database connection}

  \item{data_dir}{the directory containing the CPRD cohort
  data}

  \item{filetypes}{character vector of filetypes to be
  imported}
}
\description{
  Imports all selected CPRD data into an sqlite database
  Note that if you chose to import all the filetype, you
  may end up with aa very large database file. You may then
  chose only to import the files you want to use.  You can
  always import the rest of the files later.
}

