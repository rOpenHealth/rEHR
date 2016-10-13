#' Extracts From the database
#' 
#' This is a generic function for extracting EHR data from the database
#' 
#' The function is the base function for a range of others
#' It can either extract by itself or generate the SQL to make a query.  
#' In this way it can be combined to make compound queries.
#' The where argument is equivalent to the WHERE clause in sql
#' The elements are converted to SQL using dplyr::translate_sql_
#' If an element is wrapped in a `.()`, the element is expanded.
#' Dates should be entered as strings in ISO format (%Y-%m-%d)
#' 
#' @export
#' 
#' @param db a database connection
#' @param tab the database table to extract from
#' @param columns character vector of columns to extract from the table "*" means all tables
#' @param where sting representation of the selection criteria
#' @param sql_only logical should the function just return a string of the SQL query?
#' @param convert_dates logical should date fields be converted to R date format?
#' @return a dataframe or a string representing an sql query
#' @examples \dontrun{
#' # medical lookup tables are provided with CPRD
#' load("data/medical.RData")
#' a <- read.csv("data/chronic-renal-disease.csv")
#' a <- read_to_medcodes(a, medical, "code", lookup_readcodes= "readcode", 
#' lookup_medcodes="medcode", description = T)
#' b <- select_events(db, tab = "Referral", columns = c("patid", "eventdate", "medcode"), 
#' where = "medcode %in% .(a$medcode) & eventdate < '2000-01-01'")
#' b1 <- select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"), 
#' where = "medcode %in% .(a$medcode) & eventdate < '2000-01-01'")
#' }
select_events <- function(db = NULL, tab, columns = "*", where = NULL, 
                          sql_only = FALSE, convert_dates = FALSE){
    assert_that(is.character(tab) && length(tab) == 1)
    columns <- paste(columns, collapse = ", ")
    if(is.character(where)){
        where_clause <- translate_sql_(expand_string(where))
        sql_query <- paste("SELECT", columns, "FROM", tab, "WHERE", where_clause)
    } else sql_query <- paste("SELECT", columns, "FROM", tab)
    if(sql_only){
        sql_query
    } else {
        assert_that(class(db) == "SQLiteConnection")
        if(convert_dates){
            convert_dates(sqldf(sql_query, connection = db))
        } else sqldf(sql_query, connection = db)
    }
}


#' Selects the earliest event grouped by patient
#' 
#' This function runs a select_events() query and then groups by patient id and picks only the 
#' earliest event for each patient
#' 
#' @export
#' 
#' @param db A database connection object
#' @param tab the database table to extract from
#' @param group_column column to group by.  Default is patid
#' @param columns The other columns to be extracted
#' @param where sting representation of the selection criteria
#' @param date_column the column to sort by.  default is eventdate
#' @param sql_only logical should the function just return a string of the SQL query?
#' @return a dataframe or a string representing an sql query
#' @examples \dontrun{
#' b1 <- first_events(db, tab = "Clinical", columns = c("eventdate", "medcode"), 
#' where = "medcode %in% .(a$medcode)")
#' first_events(tab = "Clinical", columns = c("eventdate", "medcode"), 
#' where = "medcode %in% c(1, 2, 3, 4)", sql_only = TRUE)
#' }
first_events <- function(db = NULL, tab, columns = "eventdate", where = NULL, 
                         sql_only = FALSE, group_column = "patid", date_column = "eventdate"){
    other_columns <- columns[!columns %in% group_column]
    outer_columns <- paste(paste0("max(",group_column, ")"), 
                           paste(other_columns, collapse = ", "), sep = ", ")
    inner_columns <- paste(c(group_column, other_columns), collapse = ", ")
    inner_query <- select_events(db = db, tab = tab, columns = inner_columns, 
                                 where = where, sql_only = TRUE)
    inner_query <- paste(inner_query, sprintf("ORDER BY %s ASC, %s ASC", group_column, date_column))
    outer_query <- paste("SELECT", outer_columns, "FROM (", inner_query, ") GROUP BY", group_column)
    if(sql_only){
        outer_query
    } else {
        assert_that(class(db) == "SQLiteConnection")
        dat <- sqldf(outer_query, connection = db)
        names(dat)[names(dat) == sprintf("max(%s)", group_column)] <- group_column
        dat
    }
}

#' Selects the earliest event grouped by patient
#' 
#' This function runs a select_events() query and then groups by patient id and picks only the 
#' latest event for each patient
#' 
#' @export
#' 
#' @param db A database connection object
#' @param tab the database table to extract from
#' @param group_column column to group by.  Default is patid
#' @param columns The other columns to be extracted
#' @param where sting representation of the selection criteria
#' @param date_column the column to sort by.  default is eventdate
#' @param sql_only logical should the function just return a string of the SQL query?
#' @return a dataframe or a string representing an sql query
#' @examples \dontrun{
#' b2 <- last_events(db, tab = "Clinical", other_columns = c("eventdate", "medcode"), 
#' where = "medcode %in% .(a$medcode)")
#' }
last_events <- function(db = NULL, tab, columns = "eventdate", where = NULL, 
                        sql_only = FALSE, group_column = "patid", date_column = "eventdate"){
    other_columns <- columns[!columns %in% group_column]
    outer_columns <- paste(paste0("max(",group_column, ")"), 
                           paste(other_columns, collapse = ", "), sep = ", ")
    inner_columns <- paste(c(group_column, other_columns), collapse = ", ")
    inner_query <- select_events(db = db, tab = tab, columns = inner_columns, 
                                 where = where, sql_only = TRUE)
    inner_query <- paste(inner_query, sprintf("ORDER BY %s ASC, %s DESC", 
                                              group_column, date_column))
    outer_query <- paste("SELECT", outer_columns, "FROM (", inner_query, ") GROUP BY", group_column)
    if(sql_only){
        outer_query
    } else {
        assert_that(class(db) == "SQLiteConnection")
        dat <- sqldf(outer_query, connection = db)
        names(dat)[names(dat) == sprintf("max(%s)", group_column)] <- group_column
        dat
    }
}



