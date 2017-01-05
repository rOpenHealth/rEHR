   .onLoad <- function(libname, pkgname){
     set_CPRD()
     
     # CRAN Note avoidance
     if(getRversion() >= "2.15.1") 
       utils::globalVariables(
         # Variables getting the Note: "no visible binding for global variable"
         c("index_diff","prev_denom","followup","denominator","numerator","incid_denom",
           "eventdate","practid","events","birthday","tod","toreason","days","practid"
         )
       )
     invisible()
   }
