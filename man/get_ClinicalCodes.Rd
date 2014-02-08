\name{get_ClinicalCodes}
\alias{get_ClinicalCodes}
\title{Downloads clinical codes file from ClinicalCodes.org
Either specify the full path or the article id and codelist name
If an article_id is supplied but no codelist_name, all codelists are downloaded and saved as a list}
\usage{
  get_ClinicalCodes(url = NULL, article_id = NULL,
    codelist_name = NULL)
}
\arguments{
  \item{url}{character representing the url of a codelist
  download on www.clinicalcodes.org}

  \item{article_id}{integer representiong the id of a
  www.ClinicalCodes.org article}

  \item{codelist_name}{character representing a codelist
  name associated with an article on www.ClinicalCodes.org}
}
\value{
  a dataframe of clinical codes or a list of dataframes of
  clinical codes
}
\description{
  Downloads clinical codes file from ClinicalCodes.org
  Either specify the full path or the article id and
  codelist name If an article_id is supplied but no
  codelist_name, all codelists are downloaded and saved as
  a list
}

