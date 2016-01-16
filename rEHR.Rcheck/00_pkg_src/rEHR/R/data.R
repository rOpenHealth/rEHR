#' Clinical codes for 17 QOF conditions, smoking and HbA1c
#'
#' This data comes from \url{http://www.clinicalcodes.org}
#' @format A dataframe with 1283 observations:
#' \describe{
#'   \item{medcode}{Unique internal code for the medical term selected by the healthcare professional}
#'   \item{readcode}{Unique Read Code for the medical term selected by the healthcare professional}
#'   \item{desc}{Description of the code}
#'   \item{list}{Name of the clinical code list this code belongs to}
#' }
#' @source \url{http://www.clinicalcodes.org}
"clinical_codes"


#' A sample of 6 clinical tests and meaures used in UK primary care
#'
#' @format A dataframe with 6 observations:
#' \describe{
#'   \item{enttype}{Unique internal code for the entity term}
#'   \item{description}{Description of the code}
#'   \item{filetype}{The table in the EHR the entity applies to}
#'   \item{category}{Category of clinical entity}
#'   \item{data1}{first data variable}
#'   \item{data2}{second data variable}
#'   \item{data3}{third data variable}
#'   \item{data4}{fourth data variable}
#' }
#' @source \url{http://www.clinicalcodes.org}
"entity"

#' A sample of 500 medicines used in UK primary care
#'
#' @format A dataframe with 500 observations:
#' \describe{
#'   \item{prodcode}{Unique internal code for the entity term}
#'   \item{productname}{Description of the code}
#'   \item{bnfcode}{BNF code for the medicine}
#'   \item{bnfchapter}{BNF chapter heading}
#' }
#' @source \url{http://www.bnf.org/}
"product"


#' An example dataset to demonstrate the repsample function.  2474 theroetical UK GP Practices.
#'
#' @format A dataframe with 2474 observations:
#' \describe{
#'   \item{practicecode}{ID for the GP practice}
#'   \item{postcode}{postcode for the practice}
#'   \item{shacode}{Strategic Health Authorty Code}
#'   \item{shaname}{Strategic Health Authorty name}
#'   \item{pctcode}{Primary care trust code}
#'   \item{pctname}{Primary care trust name}
#'   \item{listsize}{number of patients registered at practice}
#'   \item{ftes}{ftes}
#'   \item{soaimd07}{2007 IMD deprivation score}
#'   \item{ruralvar}{Rurality}
#' }
#' @source \url{http://www.jstatsoft.org/v55/c01/paper}
"repsample_example"



#' An example EHR_definition object for defining parameters for simulating EHR data
#' 
#' @format object of type 'EHR_definition' including the following elements
#' \describe{
#'     \item{patient}{list of parameters defining the simulation of the patient table}
#'     \item{clinical}{list of parameters defining the simulation of the clinical table}
#'     \item{therapy}{list of parameters defining the simulation of the therapy table}
#'     \item{practice}{list of parameters defining the simulation of the patient table}
#'     \item{start_date}{start_date for patients - earliest possible birthday}
#'     \item{end_date}{Latest possible date in the ehr for data collection}
#' }
"ehr_def"
