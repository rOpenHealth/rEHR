#' Selected controls matching a list of variables from a case
#' 
#' Helper function for get_matches()
#' This function wil perform incidence density sampling or exact sampling up to a supplied number 
#' of matched controls
#' 
#' @export
#'  
#' @param matcher list of character strings defining the matching conditions
#' @param control_pool dataframe of potential controls for matching 
#' @param n_controls The number of controls for each case.  If replace == FALSE
#' this is a maximum value
#' @param id named vector of length 1 for the variable name and value of the case identifier
#' @param replace logical should sampling of matched controls be with replacement (incident density
#' sampling) or not.
#' @return A dataframe of matched controls or NULL if no controls could be found  
match_case <- function(matcher, control_pool, n_controls, id, replace){
    matched_controls <- filter_(control_pool, paste(names(id), "!=", id), .dots = matcher) 
    if(!nrow(matched_controls)) {
        warning("No matches for id ", id)
        return(NULL)
    }
    if(replace){
        matched_controls <- matched_controls %>% 
            sample_n(size = n_controls, replace = TRUE) %>% 
            mutate_(matched_case = id)
    } else {
        n_ <- nrow(matched_controls) 
        if(0 < n_ & n_ < n_controls) warning("Only ", n_, " controls for id ", id)
        matched_controls <- matched_controls %>% 
            sample_n(size = min(n_, n_controls), replace = FALSE) %>% 
            mutate_(matched_case = id)
    }
    matched_controls
}


#' Find matched controls for a set of cases
#' 
#' This function will provide a set of matched controls for a given set of cases.  
#' 
#' @export 
#' @param cases dataframe of cases
#' @param control_pool dataframe of potential controls to be used for matching
#' @param n_controls number of controls to match to each case
#' @param match_vars character vector of variables in the dataframes to be used to perform the 
#' matching
#' @param extra_vars character vector of other variables to be used in the matching to define other
#' conditions
#' @param id_var the variable used to identify an individual case
#' @param extra_conditions a character vector of length 1 defining further restrictions on matching
#' @param cores number of cpu cores to be used by multicore (windows users should leave set to 1)
#' @param replace logical should sampling of controls for each case be done with replacement? (see
#' details)
#' @param track logical should a dot be printed to std.out for each case?
#' @details
#' Setting replace == FALSE means that the matched cases are removed from the control pool
#' after each case has been matched.  This makes this method not thread safe and so will only 
#' run on a single core (and more slowly).
#' Setting replace == TRUE is thread safe as the same controls can be used for more than one case.
#' See Richardson (2004) Occup Environ Med 2004;61:e59 doi:10.1136/oem.2004.014472 for a description 
#' of IDS matching.
get_matches <- function(cases, control_pool, n_controls, match_vars, extra_vars, 
                        id_var, extra_conditions, cores = 6, replace = TRUE, track = TRUE){
      if(replace){
          message("Running Incident Density Sampling on ", cores, " cpu cores.")
        bind_rows(mclapply(1:nrow(cases), function(case_num){
            case <- cases[case_num,]
            matcher <- lapply(match_vars, function(m) {
                paste0(m, " == '", case[[m]], "'")  
            })
            if(!is.null(extra_conditions)) {
                matcher <- append(matcher, expand_string(extra_conditions))
            }
            id <- case[[id_var]]
            names(id) <- id_var
            if(track) cat(".")
            match_case(matcher, control_pool, n_controls, id, replace)
        }, mc.cores = cores))
    } else {
        message("Running on a single core for Unique Matching.\n", 
                nrow(control_pool), " controls in pool...")
        for(case_num in 1:nrow(cases)){
            case <- cases[case_num,]
            matcher <- lapply(match_vars, function(m) {
                paste0(m, " == '", case[[m]], "'")  
            })
            if(!is.null(extra_conditions)) {
                matcher <- append(matcher, expand_string(extra_conditions))
            }
            id <- case[[id_var]]
            names(id) <- id_var
            
            if(exists("matched_")){
                control_ids_ <- unique(matched_[[names(id)]])
                exclude_str_ <- expand_string("! .(names(id)) %in% .(control_ids_)")
                control_pool <- control_pool %>% filter_(exclude_str_) 
                if(track) cat(nrow(control_pool), " controls in pool...")
                matched_ <- bind_rows(matched_, match_case(matcher, control_pool, n_controls, id, replace))
            } else {
                matched_ <- match_case(matcher, control_pool, n_controls, id, replace)
            }
        }
        matched_
    }
}


