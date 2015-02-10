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
#' The function can also be parallelised for large datasets and chained with dply workflows. 
#' arguments should not be quoted
#' 
#' WARNING: THIS CURRENTLY ONLY WORKS FOR SINGLE TIME VARYING COVARIATES
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
#' @param on_existing see details for cutting behaviour
#'
cut_tv <- function(.data, entry, exit, cut_var, tv_name, cores = 1, on_existing = c("flip", "increment")){
    cut_individual <- function(individual){
        if(!tv_name %in% names(individual)){
            individual[[tv_name]] <- 0
        }
        if(individual[[cut_var]] > individual[[entry]] && individual[[cut_var]] < individual[[exit]]){
            pat1 <- individual
            pat1[[exit]] <- individual[[cut_var]] - 1
            pat2 <- individual
            pat2[[entry]] <- individual[[cut_var]]
            if(on_existing == "flip"){
               # message("flip")
                if (pat2[[tv_name]] == 0) pat2[[tv_name]]  <- 1 else pat2[[tv_name]] <- 0    
            } else if(on_existing == "increment") {
            #    message("inc")
                pat2[[tv_name]]  <- pat2[[tv_name]] + 1
            }
            rbind(pat1, pat2)
        } else individual
    }
    
    entry <- deparse(substitute(entry))
    exit <- deparse(substitute(exit))
    cut_var <- deparse(substitute(cut_var))
    tv_name <- deparse(substitute(tv_name))
    on_existing <- match.arg(on_existing)
    
    to_cut <- filter(.data, !is.na(.data[[cut_var]]))
    same_state <- filter(.data, is.na(.data[[cut_var]]))
    if(!tv_name %in% names(same_state) && nrow(same_state)){
        same_state[[tv_name]] <- 0
    }
    bind_rows(same_state,
              bind_rows(mclapply(1:nrow(to_cut), 
                                 function(individual){
                                     cut_individual(to_cut[individual, ])
                                 }, mc.cores = cores)))
}

# tv_test <- data.frame(id = 1:5, start = rep(0, 5), end = c(1000, 689, 1000, 874, 777), 
#                       event = c(0,1,0,1,1), drug_1 = c(NA, NA, NA, 340, 460),
#                       drug_2 = c(NA, 234, 554, 123, NA), 
#                       drug_3_start = c(110, 110,111, 109, 110),
#                       drug_3_stop = c(400, 400, 400, 400, 400),
#                       stage_1 = c(300, NA, NA, NA, NA),
#                       stage_2 = c(450, NA, NA, NA, NA))
# 
# library(testthat)
# # First tv cov
# tv_out1 <- cut_tv(tv_test, start, end, drug_1, drug_1_state)
# # 2nd tv-cov
# tv_out2 <- cut_tv(tv_out1, start, end, drug_2, drug_2_state)
# 
# expect_that(tv_out1$drug_1_state, equals(c(0,0,0,0,1,0,1)))
# expect_that(tv_out2$drug_2_state, equals(c(0,0,0,0,1,0,1,0,1,1)))
# 
# 
# 
# 
# a_tv <- cut_tv(a, start, end, psoriasis, psoriasis_state)
# a <- filter(cohort, !is.na(eventdate)) %>% slice(1:100) %>% bind_rows(slice(cohort, 1:100)) %>% select( c(1:4, 21,22,25:28)) %>% distinct(patid)
# a$scrofula <- as.Date(sapply(1:nrow(a), 
#                              function(x) sample(c(NA, 
#                                                   random_dates(1, a$start_date[x], 
#                                                                a$end_date[x])),
#                                                 size = 1)),origin="1970-01-01")
# 
# a$scrofula <- as.numeric(a$scrofula - a$start_date)
# aa <- a[!is.na(a$psoriasis) & !is.na(a$scrofula),]
# 
# tv <- cut_tv(aa,entry=start,exit = end, cut_var=psoriasis, tv_name = psoriasis_state)
# tv <- cut_tv(tv,entry=start,exit = end, cut_var=scrofula, tv_name = psoriasis_state)
# 
