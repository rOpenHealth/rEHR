#' Select patients alive and registered between certain dates
#' 
#' This function selects patients from the patient table of a CPRD database who are alive and registered within a supplied window
#' 
#' criteria are that crd is before start of window, tod is after end of window deathdate is after end of window
#' 
#' @param db a database connection
#' @param startdate character for the start of the window. format \%Y-\%m-\%d
#' @param enddate character for the end of the window. format \%Y-\%m-\%d
#' @param qs logical should only patients deemed to be of acceptable quality standard be selected? Most downloaded cohorts will be up to standard by default
#' @param registration_buffer numeric how many days must patients be registered for prior to the startdate to be included? Setting a posive value can reduce information bias
#' @param patient_tablename The name of the patient table. default is Patient
#' @export
patients_in_window <- function(db, startdate, enddate, qs = TRUE, registration_buffer = 0, patient_tablename = "Patient"){
    registration_date <- as.character(as.Date(startdate) - registration_buffer)
    selector <- sprintf("SELECT * FROM %s WHERE crd < %s AND tod > %s AND (deathdate > %s OR deathdate is NULL)",
                        patient_tablename, registration_date, enddate, enddate)
    if(qs) selector <- paste(selector, "AND accept = 1")
    sqldf(selector, connection = db)
}


