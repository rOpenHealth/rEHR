\name{import_CPRD_data}
\alias{import_CPRD_data}
\title{Imports all selected CPRD data into an sqlite database}
\usage{
import_CPRD_data(db, data_dir, filetypes = c("Additional", "Clinical",
  "Consultation", "Immunisation", "Patient", "Practice", "Referral", "Staff",
  "Test", "Therapy"), dateformat = "\%d/\%m/\%Y", yob_origin = 1800,
  regex = "PET", recursive = TRUE, ...)
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

  \item{regex}{character regular expression to identify
  data files in the directory. This is separated from the
  filetype by an underscore. e.g. 'p[0-9]{3}' in CPRD GOLD}

  \item{recursive}{logical should files be searched for
  recursively under the data_dir?}

  \item{...}{arguments to be passed to add_to_database}
}
\description{
This function can import from both cohorts downloaded via
the CPRD online tool and CPRD GOLD builds
}
\details{
Note that if you chose to import all the filetype, you may
end up with a very large database file. You may then chose
only to import the files you want to use.  You can always
import the rest of the files later. This function may take
a long time to process because it unzips (potentially
large) files, reads into R where it converts the date
formats before importing to SQLite. However, this initial
data preparation step will greatly accelerate downstream
processing.
}

