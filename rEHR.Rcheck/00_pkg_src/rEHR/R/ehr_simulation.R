#' Generates random dates between a start and end day.
#' 
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
    days <- seq.Date(start_day, end_day, by="day")  
    pick_day <- runif(n, 1, length(days))  
    days[pick_day]  
}


#' Function to simulate survival data.
#' 
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
#' 
#' The definitions of the patient file are all in the ehr_def object
#' 
#' @param ehr_def an object of class \code{EHR_definition} 
#'   
#' @export
#' 
#' @return a dataframe of simulated patients
#'   
#' @details Patients must have transferred out after the ealiest possible collection date
#' \code{ehr_def$practice$uts_limit}
#' 
simulate_ehr_patients <- function(ehr_def){
    assert_that(inherits(ehr_def, "EHR_definition"))
    ## Define birthday, sex, practid, patid
    get_patients <- function(pat_num){
        tbl_df(data.frame(birthday = random_dates(pat_num, 
                                                  ehr_def$start_date, 
                                                  ehr_def$end_date))) %>%
            mutate(yob = as.integer(format(birthday, format = "%Y")) - 1800,
                   mob = as.integer(format(birthday, format = "%m")),
                   practid = sample(ehr_def$practice$num, 
                                    pat_num, replace = TRUE), 
                   gender = sample(0:1, n(), replace = TRUE)) -> patients
        ## define presence of comorbidities
        for(comorbidity in names(ehr_def$patient$comorbidity$codes)){
            probs <- runif(nrow(patients)) < ehr_def$patient$comorbidity$prevalence[comorbidity]
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
        patients %>% 
            mutate(tod = birthday + round(365.25 * exit_sims$time, 0),
                   tod = as.Date(ifelse(tod < ehr_def$end_date, tod, NA), 
                                 origin = "1970-01-01"),
                   toreason = as.integer(exit_sims$event),
                   toreason = ifelse(toreason == 0 & !is.na(tod), 2, toreason),
                   toreason = ifelse(is.na(tod), 0, toreason),
                   deathdate = as.Date(ifelse(toreason == 1, tod, NA), 
                                       origin = "1970-01-01"),
                   # causes intermittent warnings, but bad cases are always removed in 
                   # the filter stage, hence the Horrible Hack... to fix...
                   diff = pmin(ehr_def$end_date, tod - 1, na.rm = TRUE) - birthday, 
                   days = floor(runif(pat_num, 0, as.numeric(diff))),
                   crd = birthday + days) %>% 
            filter(is.na(tod) | (tod > ehr_def$practice$uts_limit)) 
    }
    cat("Building patient table.")
    patients <- get_patients(pat_num = ehr_def$patient$num)
    while(nrow(patients) < ehr_def$patient$num){
        cat(".")
        patients_2 <- get_patients(pat_num = ehr_def$patient$num - nrow(patients))
        patients <- bind_rows(patients, patients_2)
    }
    cat("\n")
    patients %>% arrange(practid) %>% 
        group_by(practid) %>% 
        mutate(patid = as.integer(paste0(1:n(), 
                                         stringr::str_pad(practid, 
                                                          nchar(ehr_def$practice$num), 
                                                          pad = 0)))) %>% 
        ungroup() %>% 
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
    lcd <- rep(ehr_def$end_date, times = ehr_def$practice$num)
    ecd <- runif(ehr_def$practice$num) < ehr_def$practice$early_lcd_prob # early collection date
    lcd[ecd] <- lcd[ecd] - floor(runif(ehr_def$practice$num, 0, 
                                                   ehr_def$practice$early_lcd_range) * 365.25)[ecd]
    uts <- rep(ehr_def$practice$uts_limit, times = ehr_def$practice$num)
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


#' Generates simulated GP consultation tables.
#' 
#' This function generates simulated GP consultations based on an EHR_definition object and a 
#' patient table, as generated by \code{\link{simulate_ehr_patients}}.  Multicore functionality is
#' implemented via mclapply
#' 
#' @param ehr_def an object of class \code{link{EHR_definition}}
#' @param patient_table a dataframe of simulated patient EHR data
#' @param cores number of processor cores to use to run the analysis
#'   
#' @return data frame of simulated GP consultations
#'   
#' @export
#' @examples \dontrun{ patient <- simulate_ehr_patients(ehr_definition) cons <-
#'   simulate_ehr_consultations(ehr_def, patient_table = patient, cores = 4) }
#'   
simulate_ehr_consultations <- function(ehr_def, patient_table, cores = 1){
    comorbid_names <- names(ehr_def$patient$comorbidity$codes)
    
    patient_consultations <- function(p){
        comorbidities <- names(p[, comorbid_names][p[, comorbid_names] == 1])
        
        in_date <- max(as.Date("1800-01-01") +p$yob + 1800 + 18, ehr_def$practice$uts_limit)
        in_year <- as.integer(format(in_date, format = "%Y"))
        out_date <- min(p$tod, ehr_def$end_date,na.rm = TRUE)
        out_year <- as.integer(format(out_date, format = "%Y"))
        
        years <- ceiling(as.integer(out_date - in_date) / 365.25)
        
        comorbid_probs <- structure(seq(0.001, 1, length.out = years), 
                                    .Names = sapply(1:years, function(y) in_year - 1 + y))
        
        bind_rows(lapply(comorbidities, function(comorbid){
            bind_rows(lapply(1:length(comorbid_probs), function(prob){
                if(runif(1) < comorbid_probs[[prob]]){
                    cons_in_year <- rpois(1, ehr_def$consultation$per_year)
                    if(cons_in_year > 0){
                        data.frame(patid = p$patid, 
                                   practid = p$practid,
                                   eventdate = random_dates(cons_in_year, 
                                                            start_day = as.Date(paste0(names(comorbid_probs[prob]), 
                                                                                       "-01-01")),
                                                            end_day = as.Date(paste0(names(comorbid_probs[prob]), 
                                                                                     "-12-31"))),
                                   constype = sample(ehr_def$consultation$type$code, cons_in_year, 
                                                     replace = TRUE, prob = ehr_def$consultation$type$prob),
                                   comorbidity = comorbid, stringsAsFactors = FALSE)
                    } else NULL
                } else NULL
            }))
        })) %>% filter(eventdate >= in_date, eventdate < out_date)
    }
    cons <- bind_rows(parallel::mclapply(1:nrow(patient_table), function(x){
        patient_consultations(patient_table[x,])    
    }, mc.cores = cores))
    cons %>% 
        arrange(practid, eventdate) %>% 
        group_by(practid) %>%
        mutate(consid = 1:n()) %>%
        ungroup()
    
}



#' Generate simulated events tables
#' 
#' This function can generate events for clinical, referral and therapy tables.  These are based on
#' the consultation tables generated by \code{\link{simulate_ehr_consultations}}.
#' 
#' @export
#' 
#' @param ehr_def an object of class \code{link{EHR_definition}}
#' @param consultation a dataframe of simulated patient consultations
#' @param event_type Type of events to be generated
#' @param cores number of processor cores to use in generating the data
#' @param therapy_lookup lookup table for drug therapy events e.g. \code{link{product}}
#' @return dataframe
#' @details
#' This function is relatively basic - for clinical and referral tables, it generates 
#' events according to the comorbidities defined in the \code{ehr_def}, with the mean number of events 
#' for each consultation being defined in the ehr_def for that event_type.
#' For therapy events, the function simply samples the therapy_lookup table, with the mean number 
#' of events for each consultation being defined in the ehr_def for therapy.  Therefore, at the 
#' moment, the therapies bear no relationship to the conditions the patient has and are only for 
#' the purposes of explaining the functioning of the package.
#' The random sampling is based on a poisson distribution
simulate_ehr_events <- function(ehr_def, consultation, 
                                event_type = c("clinical", "referral", "therapy"), cores = 1,
                                therapy_lookup = NULL){
    
    event_type <- match.arg(event_type)
    consultation %>% mutate(events = rpois(nrow(consultation), 
                                           ehr_def[[event_type]]$mean_events)) %>%
        filter(events > 0) -> consultation
    
    bind_rows(mclapply(unique(consultation$patid), function(patient){
        pat_cons <- consultation[consultation$patid == patient,]
        if (event_type %in% c("clinical", "referral")){
            bind_rows(lapply(1:nrow(pat_cons), function(x){
                data.frame(patid = pat_cons$patid[x], eventdate = pat_cons$eventdate[x], 
                           constype = pat_cons$constype[x], consid = pat_cons$consid[x],
                           medcode = sample(ehr_def$patient$comorbidity$codes[[
                               pat_cons$comorbidity[x]]]$medcode, 
                               size = pat_cons$events[x], replace = TRUE),
                           comorbidity = pat_cons$comorbidity[x], stringsAsFactors = FALSE)
            }))
        } else if (event_type == "therapy"){
            assert_that(!is.null(therapy_lookup))
            bind_rows(lapply(1:nrow(pat_cons), function(x){
                my_products <- therapy_lookup[sample(nrow(therapy_lookup), size = pat_cons$events[x], replace = TRUE),]
                data.frame(patid = pat_cons$patid[x], eventdate = pat_cons$eventdate[x], 
                           constype = pat_cons$constype[x], consid = pat_cons$consid[x],
                           prodcode = my_products$prodcode,
                           productname = my_products$productname,
                           bnfcode = my_products$bnfcode,
                           bnfchapter = my_products$bnfchapter,
                           comorbidity = pat_cons$comorbidity[x], stringsAsFactors = FALSE)
            }))
        }
        
    }, mc.cores = cores))
    
}

