#' Calculates the prevalence totals for the output of a data frame of events/patients etc.
#' 
#' e.g. Run on the output of a call to select_by_year
#' 
#' Outputs totals by practice and/or by patient
#' 
#' @export
#' 
#' @param dat a dataframe
#' @param included_totals character vector describing which aggregates should be included c("patient", "practice")
#' @param original_data logical should the original data set be included in the output
#' @param year_var name of the variable determining year
#' @param patient_var name of the variable determining patient id
#' @param practice_var name of the variable determining practice id
#' @return list of aggregates and/or the original data
prev_totals <- function(dat, included_totals = c("patient", "practice"), original_data = TRUE,
                        year_var = "year", patient_var = "patid", practice_var = "practid"){
    out <- list()
    if("patient" %in% included_totals) out$total_counts = aggregate(dat[[patient_var]], 
                                                                   by = list(year = dat[[year_var]]), 
                                                                   FUN = length)
    if("practice" %in% included_totals) out$practice_counts = aggregate(dat[[patient_var]], 
                                                                        by = list(year = dat[[year_var]], 
                                                                        practice = dat[[practice_var]]), 
                                                                        FUN = length)
    if(original_data) out$patients <- dat
    out
}







