#' This function adds columns enabling one to calculate numerators and denominators for
#' prevalence and incidence.
#' 
#' See the vignette for more details.
#' 
#' @export
#' 
#' @param dat dataframe of longitudinal data
#' @param event_date character name of the column used to identify clinical events
#' @param year_fn function that determines how year start and end dates are calculated
#' @return longitudinal data frame with incidence, prevalence and followup columns
prev_terms <- function(dat, event_date = "eventdate", year_fn = standard_years){
  f_dates <- intersect(c(names(dat), event_date), .ehr$date_fields)
  if(length(f_dates)){
    message("Converting date columns...")
    for(column in f_dates){
      if(!is(dat[[column]], "Date")){
        dat[[column]] <- as.Date(dat[[column]])
      }
    }
  }
  start_dates <- as.Date(vapply(dat$year, function(x) year_fn(x)$startdate, "character"))
  end_dates <- as.Date(vapply(dat$year, function(x) year_fn(x)$enddate, "character"))
  
  
  dat$prev_num <- !is.na(dat[[event_date]]) & (is.na(dat[[.ehr$date_fields[["transfer_out"]]]]) | 
                                                 (dat[[.ehr$date_fields[["transfer_out"]]]] > 
                                                    dat[[event_date]]))
  dat$prev_denom <- (is.na(dat[[.ehr$date_fields[["transfer_out"]]]]) | 
                       dat[[.ehr$date_fields[["transfer_out"]]]] > start_dates)
  
  dat$incid_num <- !is.na(dat[[event_date]]) & 
    dat[[event_date]] >= start_dates & dat[[event_date]] <= end_dates &
    (is.na(dat[[.ehr$date_fields[["transfer_out"]]]]) | 
       (dat[[.ehr$date_fields[["transfer_out"]]]] > 
          dat[[event_date]]))
  dat$incid_denom <- !dat$incid_num & 
    (is.na(dat[[event_date]]) | dat[[event_date]] > end_dates) & 
    (is.na(dat[[.ehr$date_fields[["transfer_out"]]]]) | 
       dat[[.ehr$date_fields[["transfer_out"]]]] > start_dates)
  dat$followup <- pmin(end_dates, 
                       dat[[.ehr$date_fields[["transfer_out"]]]], 
                       dat[[.ehr$date_fields[["death"]]]], 
                       na.rm = TRUE) - start_dates
  dat
}


#' Calculates the prevalence totals for the output of a data frame of events/patients etc.
#' 
#' e.g. Run on the output of a call to prev_terms
#' 
#' Outputs totals by aggregated by a time variable and other variables
#' Updated to use dplyr to do the aggregation - Now 40x faster!
#' 
#' @export
#' 
#' @param dat a dataframe
#' @param included_totals character vector describing which aggregates should be included e.g. c("year", "practid")
#' @param time_var name of the variable determining timepoints
#' @param person_years numeric multiplier for presentation of prevalence and incidence numbers
#' @return list of aggregates and/or the original data
prev_totals <- function(dat, included_totals = c("year", .ehr$practice_id), time_var = "year", 
                        person_years = 100){
  prevalence <- lapply(included_totals, function(x){
    group_cols <- c(time_var, if (x != time_var) x else NULL)
    prev_den <- dat %>%
      filter(prev_denom == TRUE, followup > 0) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(denominator = sum(followup), .groups = 'drop') %>%
      mutate(denominator = as.numeric(denominator / 365.25))
    prev_num <- dat %>%
      filter(prev_num == TRUE, followup > 0) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(numerator = n(), .groups = 'drop')
    
    inner_join(prev_num, prev_den, by = group_cols) %>%
      mutate(prevalence = (numerator / (denominator / person_years)))
  })
  names(prevalence) <- paste(included_totals, "counts", sep = "_")
  
  incidence <- lapply(included_totals, function(x){
    group_cols <- c(time_var, if (x != time_var) x else NULL)
    incid_den <- dat %>%
      filter(incid_denom == TRUE, followup > 0) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(denominator = sum(followup), .groups = 'drop') %>%
      mutate(denominator = as.numeric(denominator / 365.25))
    incid_num <- dat %>%
      filter(incid_num == TRUE, followup > 0) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(numerator = n(), .groups = 'drop')
    
    inner_join(incid_num, incid_den, by = group_cols) %>%
      mutate(incidence = (numerator / (denominator / person_years)))
  })
  names(incidence) <- paste(included_totals, "counts", sep = "_")
  list(incidence = incidence, prevalence = prevalence)
}






