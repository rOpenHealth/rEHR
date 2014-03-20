#' Creates a temporary table in the database
#' 
#' This function is useful if most of your work is on a subset of the database
#' 
#' The table will exist for as long as the database connection is kept open
#' The Select_query argument will take the output from a select_events(sql_only = TRUE) based function
#' 
#' @export
#' 
#' @param db a database connection object
#' @param tab_name character name for the teporary table
#' @param select_query character the query that specifies the temporary table
#' @examples \dontrun{
#' db <- database("myCPRDdb")
#' temp_table(db, tab_name = "post_2005", 
#'            select_query = select_events(db, tab = "Referral", 
#'                                         columns = c("patid", "eventdate", "medcode"), 
#'                                         where = "eventdate > '2005-01-01'",
#'                                         sql_only = TRUE))
#' }
temp_table <- function(db, tab_name, select_query){
    sqldf(paste("CREATE TEMP TABLE", tab_name, "AS", select_query), connection = db)
    message(sprintf("Temporary table '%s' created", tab_name))
}


#' Checks if a temporary table exists and then deletes if it does
#' @param db a database connection
#' @param tab_name character the name of the table of interest
drop_temp_table <- function(db, tab_name){
    temp_names <- as.character(sqldf("SELECT name FROM sqlite_temp_master", connection=db))
    if(tab_name %in% temp_names){
        dbRemoveTable(db, tab_name)
        message(sprintf("Temporary table '%s' removed", tab_name))
    } else message(sprintf("Temporary table '%s' not found", tab_name))
}