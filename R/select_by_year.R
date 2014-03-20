#' Runs a series of selects over a year range and collects in a list of dataframes
#' 
#' This function applies a database select over a range of years and outputs as a list or a dataframe
#' The function can be parallelised using \code{multicore}.  If cores are set to 1, lapply is used without 
#' calling in multicore.
#' 
#' Because the same database connection cannot be used across threads, the input is a path to a database
#' rather than a database connection itself and a new connection is made with every fork.
#' 
#' Year start and year end criteria can be added to the where argument
#' as 'STARTDATE' and 'ENDDATE'.  These will get translated to the correct 
#' start and end dates specified by year_fn
#' 
#' @export
#' 
#' @param dbname path to the database file
#' @param tables character vector of table names
#' @param columns character vector of columns to be selected from the tables
#' @param where character string representation of the selection criteria
#' @param year_range integer vector of years to be queried
#' @param year_fn function that determines how year start and end dates are calculated
#' @param as_list logical: Should the results be returned as a list of years? If not, the data is collapsed into a dataframe
#' @param cores integer: The number of processor cores available.
#' @examples \dontrun{
#' # Output from a single table
#' where_q <- "crd < STARTDATE & (is.null(tod) | tod > ENDDATE) & accept == 1"
#' ayears <- select_by_year(db, "Patient", columns = c("patid", "yob", "tod"), where = where_q, year_range = 2000:2003)
#' # Output from multiple tables
#' load("data/medical.RData")
#' a <- read.csv("data/chronic-renal-disease.csv")
#' a <- read_to_medcodes(a, medical, "code", lookup_readcodes= "readcode", lookup_medcodes="medcode", description = T)
#' where_q <- "eventdate >= STARTDATE & eventdate <= ENDDATE & medcode %in% .(a$medcode)"
#' byears <- byears <- select_by_year("~/rOpenHealth/CPRD_test/Coupland/Coupland", c("Clinical", "Referral"), 
#' columns = c("patid", "eventdate", "medcode"), 
#' where = where_q, year_range = 2000:2003, as_list = FALSE,
#' cores = 10)
#' }
select_by_year <- function(dbname, tables, columns = "*", where, year_range, year_fn = qof_years, as_list = TRUE, cores = 1L){
    if(cores > 1){
        library(multicore)
        mylapply <- mclapply
    } else mylapply <- lapply
    dat <- mylapply(year_range, function(year, ...){
        db <- database(dbname)
        this_year <- year_fn(year)
        where_year <- str_replace_all(where, "STARTDATE", sprintf("'%s'", this_year$startdate))
        where_year <- str_replace_all(where_year, "ENDDATE", sprintf("'%s'", this_year$enddate))
        if(length(tables > 1)){
            year_out <- do.call(`rbind`, lapply(tables, function(tab){
                out <- select_events(db = db, tab = tab, columns = columns, where = where_year, sql_only = FALSE)
                out$table <- tab
                out
            }))
            year_out$year <- year
            dbDisconnect(db)
            year_out
        } else {
            year_out <- select_events(db = db, tab = tables, columns = columns, where = where_year, sql_only = FALSE)
            year_out$year <- year
            dbDisconnect(db)
            year_out
        }
    }, mc.cores = min(cores, length(year_range)))
    names(dat) <- year_range
    if(as_list){
        dat
    } else {
        do.call(`rbind`, dat)    
    }
}


#' Helper function providing startdate and enddate for a given year
#' 
#' Start and end dates matching QOF year start/ends
#' @export
#' @param year integer
qof_years <- function(year){
    list(startdate = paste0(year, "-04-01"),
         enddate = paste0(year+1, "-03-31"))
}


#' Helper function providing startdate and enddate for a given year
#' 
#' Start and end dates matching QOF year start/ends
#' @export
#' @param year integer
qof_15_months <- function(year){
    list(startdate = paste0(year, "-01-01"),
         enddate = paste0(year+1, "-03-31"))
}



#' Helper function providing startdate and enddate for a given year
#' 
#' Standard years
#' @export
#' @param year integer
standard_years <- function(year){
    list(startdate = paste0(year, "-01-01"),
         enddate = paste0(year, "-12-31"))
}
