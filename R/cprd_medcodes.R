#' Produce a dataset of CPRD medcodes with frequencies of patients in the clinical table
#' Note that this does not translate to Read/OXMIS codes
#' @param db a database connection
#' @param clinical_table name of the clinical table in the database
#' @param patid name of the patid field
#' @param medcode name of the medcode field
#' @export
#' @examples \dontrun{
#' medcode_counts <- patients_per_medcode(db)
#' head(medcode_counts)
#' } 
patients_per_medcode <- function(db, clinical_table = "Clinical", patid = "patid", medcode = "medcode"){
    sqldf(sprintf("SELECT %s, COUNT(DISTINCT %s) AS patients FROM %s GROUP BY %s",
                  medcode, patid, clinical_table, medcode), 
          connection = db)
}


