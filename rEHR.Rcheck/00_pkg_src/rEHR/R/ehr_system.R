#' Sets EHR metadata to CPRD format
#' When this is run, most functions in rEHR act as though the EHR database is CPRD
#'
#' @details  Metadata on EHR type is stored in the .ehr environment. This allows the same functions to work 
#' across different data sources. The .ehr environment is not desgined to be accessible 
#' to the user, but accessor functions are provided. CPRD is the default EHR setting.
#' @export
#' @seealso
#' get_EHR_value
#' set_EHR_value
set_CPRD <- function(){
    message("Using CPRD settings")
    .ehr <<- new.env()
    .ehr$EHR_name <- "CPRD"
    .ehr$tables <- c("Additional", "Clinical", "Consultation", 
                     "Immunisation", "Patient", "Practice", 
                     "Referral", "Staff", "Test", "Therapy")
    names(.ehr$tables) <- .ehr$tables
    # main names
    .ehr$patient_id <- "patid"
    .ehr$practice_id <- "practid"
    # clinical codes
    .ehr$ehr_medcode <- "medcode" # internal medcode name
    .ehr$lookup <- list(codes = "readcode",
                        terms = "desc",
                        tests = "description",
                        drugs = c("productname", "drugsubstance", "bnfchapter"),
                        drugcodes = "bnfcode")
     # dates
    .ehr$raw_date_format <- "%d/%m/%Y"
    .ehr$date_fields <- c("eventdate", "sysdate", "lcd", "uts", "frd", "crd", "tod", "deathdate")
    names(.ehr$date_fields) <- c("event", "entry", "last_coll", "up_to_std", "first_reg",
                                 "current_reg", "transfer_out", "death")
    .ehr$year_origin <- 1800
    .ehr$event_date <- "eventdate"
    .ehr$birth_year <- "yob"
    .ehr$cohort <- list(start_criteria = c("crd", "uts"),
                        end_criteria = c("tod", "deathdate", "lcd"))
    invisible()
 }

#' Return the value of an attribute in the .ehr environment
#' @export
#' @param x an attribute name
#' @examples {
#' set_CPRD()
#' get_EHR_attribute()
#' get_EHR_attribute(patient_id)
#' }
get_EHR_attribute <- function(x = NULL){
    x <- deparse(substitute(x))
    if(x == "NULL") x <- "EHR_name"
    get(x, envir = .ehr)
}

#' Sets the value of an attribute in the .ehr environment
#' @export
#' @param x an ehr attribute name
#' @param value the value to set to the attribute
#' @examples
#' set_CPRD()
#' set_EHR_attribute(practice_id, "pracid")
set_EHR_attribute <- function(x, value){
    x <- deparse(substitute(x))
    assign(x, value, envir = .ehr)
}

#' Lists all of the EHR attribute names in .ehr
#' @export
list_EHR_attributes <- function(){
    ls(.ehr)
}