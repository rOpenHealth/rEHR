\name{medcodes_to_read}
\alias{medcodes_to_read}
\title{Translate CPRD medcodes to Read/Oxmis}
\usage{
  medcodes_to_read(medcodes_data, lookup_table,
    medcodes_name = "medcode",
    lookup_readcodes = "readcode",
    lookup_medcodes = "medcode", description)
}
\arguments{
  \item{medcodes_data}{a dataframe with a column matching
  medcodes_name}

  \item{lookup_table}{a dataframe with columns matching
  lookup_readcodes and lookup_medcodes}

  \item{medcodes_name}{character name of the CPRD medcodes
  column in medcodes_data}

  \item{lookup_readcodes}{character name of the Read codes
  column in the lookup_table}

  \item{lookup_medcodes}{character name of the CPRD
  medcodes column in the lookup_table}

  \item{description}{logical Should description and other
  categories from the lookup table also be included?}
}
\value{
  a data frame matching the input medcodes_data with the
  Read codes and optional description columns merged in.
}
\description{
  This function accepts a data frame with a column for CPRD
  medcodes and merges with a medical lookup table to give
  columns for Read/OXMIS codes and optional descriptions
}
\details{
  Note that if the names of the medcodes columns are
  different in the data and the lookup table, the name in
  the data is retained To maintain sanity, a warning will
  be given to inform of are any name conflicts between the
  input data and the lookup
}

