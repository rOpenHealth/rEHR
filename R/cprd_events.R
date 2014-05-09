#' Selects the earliest event grouped by patient
#' 
#' This function runs a select_events() query and then groups by patient id and picks only the earliest event for each patient
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
#' b1 <- first_events(db, tab = "Clinical", columns = c("eventdate", "medcode"), where = "medcode %in% .(a$medcode)")
#' first_events(tab = "Clinical", columns = c("eventdate", "medcode"), where = "medcode %in% c(1, 2, 3, 4)", sql_only = TRUE)
#' }
first_events <- function(db = NULL, tab, columns = "eventdate", 
                         where = NULL, sql_only = FALSE, group_column = "patid", date_column = "eventdate"){
    other_columns <- columns[!columns %in% group_column]
    outer_columns <- paste(paste0("max(",group_column, ")"), paste(other_columns, collapse = ", "), sep = ", ")
    inner_columns <- paste(c(group_column, other_columns), collapse = ", ")
    inner_query <- select_events(db = db, tab = tab, columns = inner_columns, where = where, sql_only = TRUE)
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
#' This function runs a select_events() query and then groups by patient id and picks only the latest event for each patient
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
#' b2 <- last_events(db, tab = "Clinical", other_columns = c("eventdate", "medcode"), where = "medcode %in% .(a$medcode)")
#' }
last_events <- function(db = NULL, tab, columns = "eventdate", 
                        where = NULL, sql_only = FALSE, group_column = "patid", date_column = "eventdate"){
    other_columns <- columns[!columns %in% group_column]
    outer_columns <- paste(paste0("max(",group_column, ")"), paste(other_columns, collapse = ", "), sep = ", ")
    inner_columns <- paste(c(group_column, other_columns), collapse = ", ")
    inner_query <- select_events(db = db, tab = tab, columns = inner_columns, where = where, sql_only = TRUE)
    inner_query <- paste(inner_query, sprintf("ORDER BY %s ASC, %s DESC", group_column, date_column))
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


