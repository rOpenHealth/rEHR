#' Calculates the prevalence totals for the output of a data frame of events/patients etc.
#' 
#' e.g. Run on the output of a call to select_by_year
#' 
#' Outputs totals by aggregated by a time variable and other variables
#' Updated to use dplyr to do the aggregation - Now 40x faster!
#' 
#' @export
#' 
#' @param dat a dataframe
#' @param included_totals character vector describing which aggregates should be included e.g. c("year", "practid")
#' @param time_var name of the variable determining timepoints
#' @param patient_var name of the variable determining patient id for counting
#' @param original_data logical should the original data set be included in the output
#' @return list of aggregates and/or the original data
prev_totals <- function(dat, included_totals = c("year", "practid"), 
                         time_var = "year", patient_var = "patid", original_data = TRUE){
    out <- lapply(included_totals, function(x){
        if(x == time_var){
            o1 <- group_by(dat, dat[[time_var]]) %>% summarise(count = n())
            names(o1)[1] <- time_var
            o1
        } else {
            o2 <- group_by(dat, dat[[time_var]], dat[[x]]) %>% summarise(count = n()) 
            names(o2)[1:2] <- c(time_var, x)
            o2
        }
    })
    names(out) <- paste(included_totals, "counts", sep = "_")
    if(original_data) out$data <- dat
    out
}






