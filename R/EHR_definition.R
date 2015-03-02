#' Construct an EHR_definition object.
#' 
#' This function creates the object that defines EHR simulations
#' 
#' @export
#' 
#' @param start_date character date for earliest birthday of patients in EMR ("\%Y-\%m-\%d")
#' @param end_date character date for latest data collection date ("\%Y-\%m-\%d")
#' @param patient list of definitions for patient file (see details)
#' @param consultation list of definitions for consultation file (see details)
#' @param clinical list of definitions for clinical file (see details)
#' @param referral list of definitions for referral file (see details)
#' @param therapy list of definitions for therapy file (see details)
#' @param practice list of definitions for practice file (see details)
#' @details
#' The arguments for this constructor function are complex:
#' @section Patient element:
#' This is a list containing:
#' \itemize{
#'  \item num number of patients in the EHR simulation
#'  \item Comorbidity a list containing :
#'      \itemize{
#'          \item codes a named list of data frames of clinical codes in 
#'          \url{www.clinicalcodes.org} export format
#'          \item prevalence a named list of prevalences for the comorbidities
#'          \item sim_params a list containing:
#'          \itemize{
#'              \item transfer_out_prob - probability of a patient transferring out early
#'              \item scale - scaling up parameter for survival rates
#'              \item weibull_shape - parameter for survival rates
#'              \item censor_type type of censoring employed by \code{link{surv_sims}}
#'              \item betas - list of values for conditions, gender, baseline and transfer 
#'              out hazards.  The betas element is a named vector of log hazards of 
#'              length == length(comorbidity$codes) 
#'          }
#'      }
#'  \item to do.
#' }
define_EHR <- function(start_date = "1930-01-01",
                       end_date = "2014-06-30",
                       patient = list(num = 10000,
                                      comorbidity = list(codes = NULL,
                                                         prevalence =  NULL),
                                      sim_params = list(transfer_out_prob = 0.2,
                                                        scale = 25000,
                                                        weibull_shape = 1,
                                                        censor_type = "noninformative",
                                                        betas = list(conditions = NULL, #betas,
                                                                     gender = log(0.7),
                                                                     baseline = 0.01,
                                                                     transfer_out = 3.5))),
                       consultation = list(per_year = 2,
                                           type = list(code = 0:1, prob = c(0.2, 0.8))),
                       clinical = list(mean_events = 1),
                       referral = list(mean_events = 0.1),
                       therapy = list(mean_events = 1),
                       practice = list(num = 100, 
                                       regions = 13,
                                       imd_cats = 5,
                                       early_lcd_prob = 0.1,  
                                       early_lcd_range = 2,   
                                       uts_limit = as.Date("1998-01-01"),
                                       late_uts_prob = 0.5,   
                                       late_uts_range = 3)){
    obj <- structure(list(start_date = as.Date(start_date),
                          end_date = as.Date(end_date),
                          patient = patient,
                          practice = practice,
                          consultation = consultation,
                          clinical = clinical,
                          referral = referral,
                          therapy = therapy), 
                     class = c("EHR_definition", "CPRD"))
    if(is.null(obj$patient$comorbidity$codes)){
        warning("You have not defined comorbidity codes.")
    }
    if(is.null(obj$patient$comorbidity$codes)){
        warning("You have not defined comorbidity prevalences.")
    }
    if(is.null(obj$patient$sim_params$betas$conditions)){
        warning("You have not defined comorbidity condition betas (log hazards).")
    }
    obj
}


#' Tools for describing EMR_description objects.
#'  
#' @export 
#' 
#' @param x A \code{EHR_definition} object
#' @param element an element name
#' @param level nesting level for display purposes
#' @param \dots Additional arguments
print.EHR_definition <- function(x, element = NULL, level = 3, ...){
    element <- deparse(substitute(element))
    if(element == "NULL"){
        str(x, max.level = level)
    } else {
        str(x[[element]])
    }
}


