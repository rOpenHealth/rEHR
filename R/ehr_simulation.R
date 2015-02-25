#' Generates random dates between a start and end day
#' dates are in usual R as.Date() format
#' 
#' Enter start and end dates in ISO format, e.g. "2001-02-04"
#' 
#' @export
#' 
#' @param n Number of dates to be returned
#' @param start_day string representation of a start day
#' @param end_day string representation of a start day
#' @return a vector of dates
random_dates <- function(n, start_day, end_day){   
    days <- seq.Date(as.Date(start_day), as.Date(end_day), by="day")  
    pick_day <- runif(n, 1, length(days))  
    days[pick_day]  
}


#' Function to simulate survival data
#' Model: proportional hazards,
#' h(t; cov_mat, beta) = exp(cov_mat %*% beta)) / scale
#' indicators for Type I censoring (common censoring time 'tc'). 
#' 
#' @export
#' 
#' @param cov_mat n x p matrix of cov_matiates
#' @param beta p-vector of regression coefficients
#' @param cens_type typeI censoring or non-informative based on exponential distribution
#' @param baseline_hazard for modelling death dates
#' @param cens_hazard log(hazard) for non-informative censoring
#' @param cens_prob expected censoring fraction (0 <= cens_prob < 1). Used for typeI censoring
#' @param scale value to scale up the time variable by
#' @param weibull_shape shape parameter for the weibull distribution. 1 is the same as an exponential
#' @return Censored exponential survival times and censoring
#' @details
#' Weibull_shape is the k (shape) parameter from a weibull distribution 
#' \itemize{
#'    \item A value of k < 1 indicates that the mortality rate decreases over time. 
#'  This happens if there is significant infant mortality
#'    \item A value of k = 1 indicates that the mortality rate is constant over time. 
#'    This might suggest random external events are causing mortality. This is the same as an 
#'    exponential distribution
#'    \item A value of k > 1 indicates that the mortality rate increases with time. 
#'    This happens if there is an aging process.
#' }
surv_sims <- function(cov_mat, beta, cens_type = c("typeI", "noninformative"), 
                      baseline_hazard, cens_hazard = 0.04, cens_prob = 0, scale = 1, weibull_shape = 1){
    assert_that(scale > 0, is.matrix(cov_mat), cens_prob < 1)
    n <- nrow(cov_mat)
    score <- baseline_hazard * exp(cov_mat %*% -beta)
    
    tt <- rweibull(n, shape = weibull_shape, scale = score) # Uncensored survival times
    cens_type <- match.arg(cens_type)
    if (cens_type == "typeI"){
        if (cens_prob > 0){
            ## Find the common censoring time 'tc':
            type1 <- function(x) mean(exp(-x * score)) - cens_prob
            lower <- 0
            upper <- -log(cens_prob) / min(score) + 1
            tc <- uniroot(type1, c(lower, upper))$root
            ## Calculate event indicator and observed times:
            event <- tt <= tc
            time <- pmin(tt, tc)
        } else {
            event <- rep(TRUE, n)
        }    
    } else if (cens_type == "noninformative"){
        tc <- rexp(n, exp(cens_hazard))   #censoring time
        time <- pmin(tt, tc)  # observed time is min of censored and true
        event <- time == tt   
    }
    
    list(time = scale * time, event = event, tc = scale * tc)
}


#' Generate a dataframe of simulated patients with exit dates based on presented comorbidities.
#' @param ehr_def an object of class ehr_def
#' 
#' @export
#' 
#' @return a dataframe of simulated patients
#' 
#' @details
#' The definitions of the patient file are all in the ehr_def object
#'  
simulate_ehr_patients <- function(ehr_def){
    assert_that(inherits(ehr_def, "EHR_definition"))
    ## Define birthday, sex, practid, patid
    tbl_df(data.frame(birthday = random_dates(ehr_def$patient$num, 
                                              ehr_def$start_date, 
                                              ehr_def$end_date))) %>%
        mutate(yob = as.integer(format(birthday, format = "%Y")) - 1800,
               mob = as.integer(format(birthday, format = "%m")),
               practid = sample(ehr_def$practice$num, 
                                ehr_def$patient$num, replace = TRUE), 
               gender = sample(0:1, n(), replace = TRUE)) %>%
        arrange(practid) %>% 
        group_by(practid) %>% 
        mutate(patid = as.integer(paste0(1:n(), 
                                         stringr::str_pad(practid, 
                                                          nchar(ehr_def$practice$num), 
                                                          pad = 0)))) -> patients
    ## define presence of comorbidities
    for(comorbidity in names(ehr_def$patient$comorbidity$codes)){
        probs <- runif(nrow(patients)) < ehr_def$prevalence[comorbidity]
        patients[[comorbidity]] <- 0
        patients[[comorbidity]][probs] <- 1
    }
    ## Death and censoring 
    cov_mat <- as.matrix(patients[, c("gender", names(ehr_def$patient$comorbidity$codes))])
    exit_sims <- surv_sims(cov_mat, c(gender = ehr_def$patient$sim_params$betas$gender, 
                                      ehr_def$patient$sim_params$betas$conditions), 
                           cens_type = ehr_def$patient$sim_params$censor_type, 
                           cens_hazard = ehr_def$patient$sim_params$betas$transfer_out, 
                           cens_prob = ehr_def$patient$sim_params$transfer_out_prob, 
                           baseline_hazard = ehr_def$patient$sim_params$betas$baseline, 
                           scale = ehr_def$patient$sim_params$scale, 
                           weibull_shape = ehr_def$patient$sim_params$weibull_shape)
    ## define tod, toreason, deathdate, crd
    patients %>% ungroup %>% 
        mutate(tod = birthday + round(365.25 * exit_sims$time, 0),
               tod = as.Date(ifelse(tod < as.Date(ehr_def$end_date), tod, NA), 
                             origin = "1970-01-01"),
               toreason = as.integer(exit_sims$event),
               toreason = ifelse(toreason == 0 & !is.na(tod), 2, toreason),
               toreason = ifelse(is.na(tod), 0, toreason),
               deathdate = as.Date(ifelse(toreason == 1, tod, NA), 
                                   origin = "1970-01-01"),
               diff = pmin(as.Date(ehr_def$end_date), tod - 1, na.rm = TRUE) - birthday,
               days = floor(runif(ehr_def$patient$num, 0, as.numeric(diff))),
               crd = birthday + days) %>%
        select_(.dots = c("patid", "practid", "gender", "yob", "mob", "crd", "tod", 
                          "toreason", "deathdate", names(ehr_def$patient$comorbidity$codes))) 
}


#' generates a simulated dataframe of primary care practices in the same format as is used in CPRD
#' @param ehr_def an object of class ehr_def
#' 
#' @export
#' 
#' @return a dataframe of simulated practices
#' 
#' @details
#' The definitions of the practice file are all in the ehr_def object
simulate_ehr_practices <- function(ehr_def){
    assert_that(inherits(ehr_def, "EHR_definition"))
    region <- sample(1:ehr_def$practice$regions, size = ehr_def$practice$num, replace = TRUE)
    imd_5 <- sample(1:ehr_def$practice$imd_cats, size = ehr_def$practice$num, replace = TRUE)
    lcd <- as.Date(rep(ehr_def$end_date, times = ehr_def$practice$num))
    ecd <- runif(ehr_def$practice$num) < ehr_def$practice$early_lcd_prob # early collection date
    lcd[ecd] <- lcd[ecd] - floor(runif(ehr_def$practice$num, 0, 
                                                   ehr_def$practice$early_lcd_range) * 365.25)[ecd]
    uts <- as.Date(rep(ehr_def$practice$uts_limit, times = ehr_def$practice$num))
    lts <- runif(ehr_def$practice$num) < ehr_def$practice$late_uts_prob # late to standard
    uts[lts] <- uts[lts] + floor(runif(ehr_def$practice$num, 0, 
                                       ehr_def$practice$early_lcd_range) * 365.25)[lts]
    lcd[lcd <= uts] <- ehr_def$end_date
    tbl_df(data.frame(practid = 1:ehr_def$practice$num,
                      region = region,
                      lcd = lcd,
                      uts = uts,
                      imd_5 = imd_5))
}


