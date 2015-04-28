#' Converts a longitudinal data set from e.g. \code{prev_terms} to a cohort dataset.
#' The dataset has a single row for each patient and includes only patients in the numerator or 
#' denominator for whichever cohort type is chosen. Columns are added for start and end dates and 
#' for start and end times as integer differences from the cohort start date. A binary column is 
#' added for membership of the case group.   All patients
#' with start date > end date are removed from the dataset.
#' 
#' @export
#' 
#' @param dat dataframe as output from a call to \code{prev_terms}
#' @param cohort_type character either 'incid' or 'prev'.  This selects the numerators and 
#' denominators for that type of cohort
#' @param cohort_start ISO date character string for the start of the cohort
#' @param cohort_end ISO date character string for the end of the cohort
#' @param diagnosis_start character string for the name of the diagnosis variable used to define the 
#' start dates, or NULL if the diagnosis date is not to be included in the definition 
#' of start dates.
build_cohort <- function(dat, cohort_type = c("incid", "prev"), cohort_start, 
                         cohort_end, diagnosis_start = "eventdate"){
    cohort_type <- match.arg(cohort_type)
    cohort_filter <- wrap_sql_query("#1_num |  #1_denom", cohort_type)
    cohort_start <- as.Date(cohort_start)
    cohort_end <- as.Date(cohort_end)
    end_criteria <- intersect(.ehr$cohort$end_criteria, names(dat))
    if(!is.null(diagnosis_start)){
        start_criteria <- c(diagnosis_start, intersect(.ehr$cohort$start_criteria, names(dat)))
    } else {
        start_criteria <- intersect(.ehr$cohort$start_criteria, names(dat))
    }
    start_q <- paste0("as.Date(pmax(as.Date('", 
                      cohort_start, "'), ", 
                      paste(start_criteria, 
                            collapse = ", ", sep = ","), ", na.rm = TRUE), origin = '1970-01-01')",
                      collapse = ",")
    end_q <- paste0("as.Date(pmin(as.Date('", 
                    cohort_end, "'), ", 
                    paste(end_criteria, 
                          collapse = ", ", sep = ","), ", na.rm = TRUE), origin = '1970-01-01')", 
                    collapse = ",")
     dat %>% 
        filter_(cohort_filter) %>% 
        arrange_(.ehr$patient_id, "desc(year)") %>% 
        distinct_(.ehr$patient_id)  %>% 
        mutate_(start_date = start_q,
                end_date = end_q,
                start = paste0("as.integer(start_date - as.Date('", cohort_start, "'))"),
                end = paste0("as.integer(end_date - as.Date('", cohort_start, "'))"),
                case = sprintf("ifelse(%s_num, 1, 0)", cohort_type)) %>%
        filter(start < end) 
        
                
}

#' cut_tv - Cuts a survival dataset on a time varying variable
#'
#' Survival datasets often have time-varying covariates that need to be dealt with. For example
#' a drug exposure may occur after the entry into the cohort and you are interested in
#' how this might affect your outcome.
#' 
#' This function cuts up a dataset based on times supplied for the time-varying covariate. If there
#' is already a variable for the time-varying covariate, you can chose to flip the existing values 
#' or increment them.  This means the function can be called multiple times to, e.g. deal with
#' drugs starting and stopping and also to deal with progression of treatment.
#' 
#' The function is faster than other cutting methods, does not require conversion to Lexis format,
#' and can be parallelised for large datasets and chained with dply workflows. 
#' Arguments should not be quoted.
#' 
#' 
#' @export
#' 
#' @param .data a dataframe
#' @param entry name of the column in .data that defines entry time to cohort. Column must be 
#' numeric.  
#' @param exit name of the column in .data that defines exit time from cohort. 
#' Column must be numeric
#' @param cut_var name of the column in .data that defines the time of the time-varying covariate 
#' event. Column must be numeric.
#' @param tv_name name for the constructed time-varying covariate
#' @param cores number of mc.cores to use.
#' @param id_var name of the variable identifying individual cases
#' @param on_existing see details for cutting behaviour
#' @details
#' This function can deal with the following scenarios (see examples):
#'  \itemize{
#'  \item{"Binary chronic covariates"}{e.g. The time of diagnosis for a chronic (unresolvable)
#'   condition. This requires a single column variable of times from entry in the dataset}
#'  \item{"Binary covariates"}{e.g. times of starting and stopping medication.  This requires
#'  more than one column variable in the dataset, one for each start or stop event.  The state flips
#'  with each new change.}
#'  \item{"Incremental time-varying covariates"}{e.g. different stages of a condition.  This 
#'  requires a single column variable for each incremental stage}
#'  \item{"Any combination of the above"}{This is achieved by chaining multiple calls together}
#' } 
#' @examples
#' # A simple example dataset to be cut
#' tv_test <- data.frame(id = 1:5, start = rep(0, 5), end = c(1000, 689, 1000, 874, 777), 
#'                       event = c(0,1,0,1,1), drug_1 = c(NA, NA, NA, 340, 460),
#'                       drug_2 = c(NA, 234, 554, 123, NA), 
#'                       drug_3_start = c(110, 110,111, 109, 110),
#'                       drug_3_stop = c(400, 400, 400, 400, 400),
#'                       stage_1 = c(300, NA, NA, NA, NA),
#'                       stage_2 = c(450, NA, NA, NA, NA))
#'
#' # Binary chronic covariates:
#' tv_out1 <- cut_tv(tv_test, start, end, drug_1, id_var = id, drug_1_state)
#' tv_out1 <- cut_tv(tv_out1, start, end, drug_2, id_var = id, drug_2_state)
#' # Binary covariates:
#' tv_out3 <- cut_tv(tv_test, start, end, drug_3_start, id_var = id, drug_3_state)
#' tv_out3 <- cut_tv(tv_out3, start, end, drug_3_stop, id_var = id, drug_3_state)
#' # incremental covariates:
#' inc_1 <- cut_tv(tv_test, start, end, stage_1, id_var = id, disease_stage, on_existing = "inc")
#' inc_1 <- cut_tv(inc_1, start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
#' # Chaining combinations of the above 
#' \dontrun{
#' library(dplyr)
#' tv_all <- tv_test %>%
#'           cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>% 
#'           cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
#'           cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
#'           cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
#'           cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc") %>%
#'           cut_tv(start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
#' } 
#' 
cut_tv <- function(.data, entry, exit, cut_var, tv_name, cores = 1, 
                   id_var, on_existing = c("flip", "increment")){
    cut_individual <- function(individual){
        if(!tv_name %in% names(individual)){
            individual[[tv_name]] <- 0
        }
        for(i in 1:nrow(individual)){
            if(i == 1){
                out <- cut_row(individual[i, ], prior_state = 0)
            } else {
                out <- bind_rows(out, cut_row(individual[i,], 
                                              prior_state = slice(out, n())[[tv_name]]))
            }
        }
        out
    }
    cut_row <- function(r, prior_state){
        if(r[[cut_var]] > r[[entry]] && r[[cut_var]] < r[[exit]]){
            pat1 <- r
            pat1[[exit]] <- r[[cut_var]] - 1
            pat2 <- r
            pat2[[entry]] <- r[[cut_var]]
            if(on_existing == "flip"){
                if (pat2[[tv_name]] == 0) pat2[[tv_name]]  <- 1 else pat2[[tv_name]] <- 0    
            } else if(on_existing == "increment") {
                pat2[[tv_name]]  <- pat2[[tv_name]] + 1
            }
            rbind(pat1, pat2)
        } else {
            if(on_existing == "increment" & r[,tv_name] > 0) {
                r
            } else {
                r[,tv_name] <- prior_state
                r
            }
        } 
    }
    
    entry <- deparse(substitute(entry))
    exit <- deparse(substitute(exit))
    cut_var <- deparse(substitute(cut_var))
    tv_name <- deparse(substitute(tv_name))
    id_var <- deparse(substitute(id_var))
    on_existing <- match.arg(on_existing)
    
    to_cut <- filter(.data, !is.na(.data[[cut_var]]))
    same_state <- filter(.data, is.na(.data[[cut_var]]))
    if(!tv_name %in% names(same_state) && nrow(same_state)){
        same_state[[tv_name]] <- 0
    }
    bind_rows(same_state,
              bind_rows(mclapply(unique(to_cut[[id_var]]), 
                                 function(x){
                                     cut_individual(to_cut[to_cut[[id_var]] == x, ])
                                 }, mc.cores = cores))) %>%
        arrange_(id_var, entry)
}








