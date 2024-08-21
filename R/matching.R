#' Selected controls matching a list of variables from a case
#' 
#' Helper function for get_matches()
#' This function wil perform incidence density sampling or exact sampling up to a supplied number 
#' of matched controls
#'  
#'  
#' @param matcher list of character strings defining the matching conditions
#' @param control_pool dataframe of potential controls for matching 
#' @param n_controls The number of controls for each case.  If replace == FALSE
#' this is a maximum value
#' @param id named vector of length 1 for the variable name and value of the case identifier
#' @param replace logical should sampling of matched controls be with replacement (incident density
#' sampling) or not.
#' @return A dataframe of matched controls or NULL if no controls could be found  
match_case <- function(matcher, control_pool, n_controls, id, replace) {
  id_name <- names(id)
  id_value <- id[[1]]

  filter_expr <- rlang::expr((!!rlang::sym(id_name) != !!id_value))
  for (cond in matcher) {
    filter_expr <- rlang::expr(!!filter_expr & !!cond)
  }

  matched_controls <- dplyr::filter(control_pool, !!filter_expr)
  
  if(nrow(matched_controls) == 0) {
    cat("No matches for id", id, ".")
    return(NULL)
  }
  
  if(replace){
    matched_controls <- dplyr::sample_n(matched_controls, size = n_controls, replace = TRUE)
  } else {
    n_ <- nrow(matched_controls)
    if(0 < n_ && n_ < n_controls) cat(n_)
    matched_controls <- dplyr::sample_n(matched_controls, size = min(n_, n_controls), replace = FALSE)
  }
  dplyr::mutate(matched_controls, matched_case = id_value)
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
#' @param extra_conditions a character vector of length 1 defining further restrictions on matching
#' @param cores number of cpu cores to be used by multicore (windows users should leave set to 1)
#' @param method The method of selection of controls (see details)
#' @param track logical should a dot be printed to std.out for each case?
#' @param tracker function to track progress of the function (See details)
#' @param diagnosis_date character the name of the variable in the cases and control_pool datasets
#' containing the date of diagnosis (or other event to base the IDM method on).  If there is no
#' diagnosis date for a patient, this should be represented by NA
#' @details
#' Setting method to "exact" means that the matched controls are removed from the control pool
#' after each case has been matched.  This makes this method not thread safe and so will only 
#' run on a single core (and more slowly).
#' Setting method to "incidence_density" is thread safe as the same controls can be used for more than one case.
#' See Richardson (2004) Occup Environ Med 2004;61:e59 doi:10.1136/oem.2004.014472 for a description 
#' of IDS matching.  Also see the introduction vignette.
#' The tracker variable allows for different outputs to track the progress of the function.
#' This is currently set to ouput a dot for every case matched.  A function can be added to the 
#' argument For a more verbose tracking, e.g. to track number of cases, 
#' set \code{tracker = function() paste0(case_num, ",")}
get_matches <- function(cases, control_pool, n_controls, match_vars, extra_vars, 
                        extra_conditions = NULL, cores = 1, 
                        track = TRUE, tracker = function(case_num) ".", 
                        method = c("incidence_density", "exact"), 
                        diagnosis_date = NULL) {
  method <- match.arg(method)
  
  if (method == "incidence_density") {
    assert_that(!is.null(diagnosis_date))
    message("Running Incident Density Sampling on ", cores, " cpu cores.")
    cases$pcase <- 1
    control_pool$pcase <- 0
    control_pool <- bind_rows(cases, control_pool)
    
    results <- mclapply(1:nrow(cases), function(case_num) {
      CASE <- cases[case_num, ]
      matcher <- lapply(match_vars, function(m) {
        rlang::expr(!!rlang::sym(m) == !!CASE[[m]])
      })
      if (!is.null(extra_conditions)) {
        sql_expr <- wrap_sql_query("is.na(#1) | #1 > '#2'", diagnosis_date, CASE[[diagnosis_date]])
        matcher <- c(matcher, rlang::parse_expr(sql_expr))
      }
      id <- setNames(list(CASE[[.ehr$patient_id]]), .ehr$patient_id)
      if (track) cat(tracker(case_num))
      match_case(matcher, control_pool, n_controls, id, replace = TRUE)
    }, mc.cores = cores)
    do.call(dplyr::bind_rows, results)
  } else if (method == "exact"){
      message("Running on a single core for Unique Matching.\n", nrow(control_pool), " controls...")
      matched_ <- NULL
      for (case_num in 1:nrow(cases)) {
        if (track) cat(tracker(case_num))
        CASE <- cases[case_num, ]
        matcher <- lapply(match_vars, function(m) {
          rlang::expr(!!rlang::sym(m) == !!CASE[[m]])
        })
        if (!is.null(extra_conditions)) {
          matcher <- append(matcher, rlang::parse_exprs(extra_conditions))
        }
        id <- setNames(list(CASE[[.ehr$patient_id]]), .ehr$patient_id)
        matched_case <- match_case(matcher, control_pool, n_controls, id, replace = FALSE)
        matched_ <- if (is.null(matched_)) matched_case else dplyr::bind_rows(matched_, matched_case)
        
        control_ids_ <- unique(matched_[[names(id)]])
        exclude_str_ <- sprintf("!('%s' %%in%% c(%s))", names(id), paste(control_ids_, collapse=", "))
        
        control_pool <- dplyr::filter(control_pool, !!rlang::parse_expr(exclude_str_))
      }
      matched_
    }
}

#' Function for performing matching of controls to cases using the consultation files to generate a
#' dummy index date for controls.
#' 
#'  Controls are matched on an arbitrary number of categrorical variables and on continuous 
#'  variables via the \code{extra_conditions} argument. Also the date at \code{index_var} is matched to the 
#'  eventdate in the consultation files, providing a dummy index date for controls of a consultaton
#'  within +/- \code{index_diff_limit} days of the index date.
#'  
#'  Note that the consultaton files must be in flat-file format (i.e. not as part of the database, 
#'  but as text (or other filetype, e.g stata dta) files).  Set the \code{import_fn} argument to 
#'  use different file formats (e.g. \code{foreign::read.dta} or \code{readstata13::read.dta13})
#'  
#'  The \code{extra_conditions} argument can add extra condtions to the matching criteria
#'  on top of the matching vars for example you could add "year > 1990".  You can wrap calls to 
#'  expressions in dotted brackets to automatically expand them.  This is particularly useful
#'  when you want to find the value for each individual case. Each case is denoted by \code{CASE}
#'  e.g. "start_date < .(CASE$start_date)" will ensure the start date for controls is prior to 
#'  the start date for the matched case.
#'  
#'   
#' 
#' @export
#' 
#' @param cases A dataframe of cases to which to match controls
#' @param control_pool A dataframe of possible contols to match to cases
#' @param index_var character string of the name of the variable containing index dates
#' @param match_vars character vector detailing the common variables in \code{cases} and 
#' \code{control_pool} to match on
#' @param extra_conditions character string detailing other matching constraints (see details)
#' @param index_diff_limit integer number of days before or after the case index date that 
#' dummy index dates can be picked from the consultation files
#' @param consult_path path to directory containing consultation files
#' @param n_controls integer the number of controls to attempt to match to each case
#' @param cores integer the number of processor cores to be used in processing
#' @param import_fn function name stipulating the function used to read the consultation files
#' @param \dots extra arguments to be passed to import_fn
#' 
#' @return a dataframe of matched controls
#' 
match_on_index <- function(cases, control_pool, index_var, match_vars,
                           extra_conditions = "", index_diff_limit = 90, consult_path,
                           n_controls = 5, cores = 1, import_fn = read.delim, ...){
  message("Finding ", nrow(cases), " cases...")
  cons_files <- list.files(consult_path)
  bind_rows(mclapply(unique(cases[[ .ehr$practice_id ]]), function(practice){
    p_cases <- filter_(cases, expand_string(".(.ehr$practice_id) == .(practice)"))
    p_controls <- filter_(control_pool, expand_string(".(.ehr$practice_id) == .(practice)"))
    #cons_files[str_detect(cons_files, pattern = str_pad(practice, 3, pad = 0))]
    consultations <- import_fn(file.path(consult_path, 
                                         cons_files[str_detect(cons_files, 
                                                               str_pad(practice, 
                                                                       3, pad = 0))]), ...) %>%
      filter_(expand_string("! .(.ehr$patient_id) %in% .(p_cases[[.ehr$patient_id]])")) 
    if(exists("matched_")) rm(matched_)
    p_num <- nrow(p_cases)
    cat("Start practice", practice, ".")
    for(case_num in 1:p_num){
      CASE <- p_cases[case_num,]
      extra_conditions_ <- expand_string(extra_conditions)
      matcher <- lapply(match_vars, function(m) {
        paste0(m, " == '", CASE[[m]], "'")  
      })
      if(nchar(extra_conditions_)){
        matcher <- append(matcher, extra_conditions_, after = length(matcher))
      }
      id <- CASE[[ .ehr$patient_id]]
      names(id) <- .ehr$patient_id
      matched_controls <- filter_(p_controls, .dots = append(paste(names(id), "!=", id), 
                                                             matcher)) 
      matched_consultations <- filter_(consultations, 
                                       expand_string(".(.ehr$patient_id) %in% 
                                                    .(matched_controls[[.ehr$patient_id]])")) %>% 
        mutate_(index_diff = expand_string("abs(as.integer( .(.ehr$eventdate) - 
                                                   .(CASE[[index_var]]) ))")) %>%
        filter(index_diff <= index_diff_limit) %>% 
        group_by_(expand_string(".(.ehr$patient_id)")) %>% 
        arrange(index_diff) %>%
        distinct() %>%
        transmute_(index_date = expand_string(".(.ehr$eventdate)"))
      matched_controls <- inner_join(matched_controls, matched_consultations, 
                                     by = .ehr$patient_id)
      n_ <- nrow(matched_controls)
      if (n_ == 0){
        cat(0)
        #next
      } else if (0 < n_ & n_ < n_controls){
        cat(n_)
      } else cat(".")
      if(exists("matched_")){
        matched_ <- bind_rows(matched_, matched_controls %>%
                                sample_n(size = min(n_, n_controls), replace = FALSE) %>% 
                                mutate_(matched_case = id))
      } else {
        matched_ <- matched_controls %>%
          sample_n(size = min(n_, n_controls), replace = FALSE) %>% 
          mutate_(matched_case = id)
      }
      control_ids_ <- unique(matched_[[names(id)]])
      exclude_str_ <- expand_string("! .(names(id)) %in% .(control_ids_)")
      p_controls <- p_controls %>% filter_(exclude_str_) 
      
    }
    cat("Practice", practice, "exhausted.")
    matched_
  }, mc.cores = cores))
}


# Check and load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required but not installed.")
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Package 'stringdist' is required but not installed.")
}

library(dplyr)
library(stringdist)

#' Calculate similarity between case and control
#'
#' @param case_row A dataframe row representing a single case
#' @param control_row A dataframe row representing a single control
#' @param match_vars A character vector of variable names to match on
#' @return A numeric value representing the similarity score
calculate_similarity <- function(case_row, control_row, match_vars) {
  similarity_scores <- sapply(match_vars, function(var) {
    1 - stringdist::stringdist(case_row[[var]], control_row[[var]], method = "jaccard")
  })
  mean(similarity_scores)
}

#' Probabilistic match for a single case
#'
#' @param case_row A dataframe row representing a single case
#' @param control_pool A dataframe of potential controls
#' @param n_controls The number of controls to match to each case
#' @param match_vars A character vector of variable names to match on
#' @param threshold A numeric value for the minimum similarity score required
#' @return A dataframe of matched controls or NULL if no controls could be found
probabilistic_match_case <- function(case_row, control_pool, n_controls, match_vars, threshold = 0.5) {
  control_pool <- control_pool %>%
    mutate(similarity = calculate_similarity(case_row, ., match_vars)) %>%
    filter(similarity >= threshold) %>%
    arrange(desc(similarity)) %>%
    head(n_controls)
  control_pool %>%
    mutate(matched_case = case_row$patid)
}

#' Selected controls matching a list of variables from a case
#' 
#' Helper function for probabilistic_get_matches()
#' This function will perform probabilistic matching up to a supplied number 
#' of matched controls.
#'  
#' @param matcher list of character strings defining the matching conditions
#' @param control_pool dataframe of potential controls for matching 
#' @param n_controls The number of controls for each case. If replace == FALSE
#' this is a maximum value
#' @param id named vector of length 1 for the variable name and value of the case identifier
#' @param replace logical should sampling of matched controls be with replacement (incident density
#' sampling) or not.
#' @return A dataframe of matched controls or NULL if no controls could be found  
#' @examples \dontrun{
#' cases <- data.frame(patid = 1:3, name = c("Alice", "Bob", "Charlie"), age = c(25, 35, 45), gender = c("F", "M", "M"))
#' control_pool <- data.frame(patid = 4:10, name = c("Alicia", "Robert", "Charles", "Al", "Bobby", "Char", "Ali"), age = c(24, 36, 44, 26, 34, 46, 27), gender = c("F", "M", "M", "F"))
#' matched_controls <- probabilistic_get_matches(cases, control_pool, n_controls = 2, match_vars = c("name", "age", "gender"), threshold = 0.5, cores = 1)
#' print(matched_controls)
#' }
probabilistic_get_matches <- function(cases, control_pool, n_controls, match_vars, threshold = 0.5, cores = 1) {
  cases <- mutate(cases, case_id = row_number())
  results <- do.call(rbind, parallel::mclapply(seq_len(nrow(cases)), function(case_num) {
    case_row <- cases[case_num,]
    probabilistic_match_case(case_row, control_pool, n_controls, match_vars, threshold)
  }, mc.cores = cores))
  
  results
}
