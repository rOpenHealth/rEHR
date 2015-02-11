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

#' combines strings and vectors in a sensible way for select queries
#' 
#' This function is a variant of the sprintf function.
#' In the query, can be placed identifier tags which are a hash character followed by a number 
#' e.g. #1
#' The number in the tag reflects the position of the arguments after the query
#' The resut of evaluating that argument will then be inserted in place of the tag.
#' If the result of evaluating the argument is a vectr of length 1, it is inserted as is.
#' If it is a vector of length > 1, it is wrapped in parentheses and comma separated.  
#' 
#' Note that this function is for help in constructing raw SQL queries and should not be used as an 
#' input to the \code{where} argument in \code{select_event} calls.
#' This is because these calls use translate_sql_q to translate from R code to SQL
#' 
#' @export
#' 
#' @param query a character string with identifier tags (#[number]) for selecting the argument in \dots
#' @param \dots optional arguments selected by the identifier tags
#' @examples
#' medcodes1 <- 1:5
#' practice <- 255
#' wrap_sql_query("eventdate >= STARTDATE & eventdate <= ENDDATE & medcode %in% #1 & 
#'    practice == #2", medcodes1, practice)
wrap_sql_query <- function(query, ...){
    items <- list(...)
    if(!length(items)) return(query)
    items <- lapply(items, function(x){
        if(length(x) > 1){
            paste("(", paste(x, collapse = ", "), ")")
        } else x
    })
    locations <- unique(unlist(str_extract_all(query, "#[0-9]+")))
    max_locations <- max(as.numeric(unlist(str_extract_all(locations, "[0-9]+"))))
    assert_that(length(items) == max_locations)
    items_dict <- list()
    for(l in 1:length(locations)){
        items_dict[[locations[l]]] <- items[[as.integer(str_extract(locations[l], "[0-9]+"))]]
    }
    for(n in names(items_dict)){
        query <- str_replace_all(query, n, items_dict[[n]])
    }
    query
}


#' Reads strings and expands sections wrapped in dotted parentheses
#' 
#'  This is a kind of inverse of bquote
#'  @export
#'  @param s a string
#'  @param level integer sets the parent frame level for evaluation
#'  @examples
#'  a <- runif(10)
#'  expand_string("The r code is .(a)")
expand_string <- function(s, level = 3){
    e <- strsplit(s, "[[:space:]]+")[[1]]
    paste(lapply(e, 
                 function(x){
                     if(str_detect(x, "^\\.")){
                         eval(parse(text = str_match(x, "\\.(.+)")[2]),
                              envir = parent.frame(n = level))
                     } else x
                 }), collapse = " ")
}
