#' Compresses a dataframe to make more efficient use of resources
#' 
#' Converts date variables in a dataframe to integers
#' Integers represent time in days from the supplied origin 
#' Converts specified numeric values to integer
#' This function is useful for keeping file sizes down
#' 
#' @export
#' 
#' @param dat a dataframe
#' @param origin ISO string representation of the dat of origin.  default is UNIX start date
#' @param format character: format of the date string.  Default is ISO standard
#' @param date_fields character vector of column names representing dates
#' @param integer_fields character vector of column names that should be integers
#' @return dataframe
compress <- function(dat, origin = "1970-01-01", format = "%Y-%m-%d",
                           date_fields = c("eventdate", "sysdate", "lcd", "uts", 
                                           "frd", "crd", "tod", "deathdate"),
                     integer_fields = c("yob", "practid")){
    date_to_int <- function(x){
        if(class(x) == "character") x <- as.Date(x, format = format)
        if(class(x) == "Date") as.integer(x - as.Date(origin))
    }
    message("compressing...")
    for(n in intersect(date_fields, names(dat))){
        dat[[n]] <- date_to_int(dat[[n]])
    }
    for(n in intersect(integer_fields, names(dat))){
        if(class(dat[[n]]) != "integer") dat[[n]] <- as.integer(dat[[n]])
    }
    dat
}
    
#' Compresses a dataframe and saves in stata format
#' 
#' Defaults to saving compressed dates to integer days from 1960-01-01
#' which is the standard in stata.
#' 
#' @export
#'  
#' @param dat dataframe
#' @param fname character string: filepath to save to
#' @param \dots arguments to be passed to compress
to_stata <- function(dat, fname, ...){
    write.dta(compress(dat, origin = "1960-01-01", ...), fname)
    message(sprintf("Dataframe %s exported to %s", deparse(substitute(dat)), fname))
}



