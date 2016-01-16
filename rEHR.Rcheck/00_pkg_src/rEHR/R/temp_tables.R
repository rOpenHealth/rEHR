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
    dbGetQuery(db, paste("CREATE TEMP TABLE", tab_name, "AS", select_query, ";"))
    message(sprintf("Temporary table '%s' created", tab_name))
}


#' Appends rows to a temporary table
#' 
#' This function checks if a table is a temp table and then adds to it based on the select statement
#' The check is to maintain the integrity of the database
#' 
#' @export
#' 
#' @param db a database connection
#' @param tab_name the name of the temporary table being appended to
#' @param columns character vector of columns in tab_name
#' @param select_query SQL query for the selector
append_to_temp_table <- function(db, tab_name, columns, select_query){
    temp_names <- as.character(sqldf("SELECT name FROM sqlite_temp_master", connection=db)$name)
    if(tab_name %in% temp_names){
        message("Inserting...", appendLF = FALSE)
        sqldf(paste("INSERT INTO", tab_name, "(", paste(columns, collapse = ", "),  ")", select_query), connection = db)
        message(sprintf("Finished appending to temporary table '%s'.", tab_name))
    } else message(sprintf("%s is not a temporary table in this database!"), tab_name)
}



#' Send a dataframe to a temporary table in the database
#' 
#' The table is a temporary database and is linked only to the current connection object
#' 
#' @export
#' 
#' @param db a database connection
#' @param tab_name character name for the new temporary database table
#' @param dat dataframe to send to the temporary database table
#' @param overwrite logical if a table already exists with the same name should it be dropped? 
to_temp_table <- function(db, tab_name, dat, overwrite = FALSE){
    temp_tables <- unlist(dbGetQuery(db, "select name from sqlite_temp_master;"))
    if(overwrite && tab_name %in% temp_tables){
        drop_temp_table(db, tab_name)
    } else if(tab_name %in% temp_tables){
        message("Temp table ", tab_name, " already exists - No action taken")
        return(invisible())
    }
    temp_tab_name <- paste("temp", tab_name, sep = ".")
    dbWriteTable(db, name = temp_tab_name, value = dat, row.names = FALSE) 
    dat_name <- deparse(substitute(dat))
    message("Sent ",  dat_name, " to temporary database ", tab_name)
}


#' Checks if a temporary table exists and then deletes if it does
#' 
#' @export
#' 
#' @param db a database connection
#' @param tab_name character the name of the table of interest
drop_temp_table <- function(db, tab_name){
    temp_names <- as.character(sqldf("SELECT name FROM sqlite_temp_master", connection=db)$name)
    if(tab_name %in% temp_names){
        dbRemoveTable(db, tab_name)
        message(sprintf("Temporary table '%s' removed", tab_name))
    } else message(sprintf("Temporary table '%s' not found", tab_name))
}

#' drops all temporary tables from the database
#' 
#' This is useful for temporary storage/memory management
#' 
#' @export
#' 
#' @param db a database connection
drop_all_temp_tables <- function(db){
    temp_tables <- head(db, temp = TRUE)$name
    if(length(temp_tables)){
        for(tab_name in temp_tables){
            dbRemoveTable(db, tab_name)
            message(sprintf("Temporary table '%s' removed", tab_name))
        }
    } else message("No temporary tables in database to remove")
}

#' Sets location of the db temporary store for temporary tables
#' 
#' By default, sqlite stores temp tables in /tmp (Or windows equivalent). If you are building large temporary tables 
#' and don't have a large /tmp directory, you can get "database or disk is full" errors.
#' If you have a lot of RAM you can set \code{store} to "RAM" and the temp files will be stored in RAM
#' rather than in /tmp.  This could also speed things up.
#' 
#' @export
#' 
#' @param db a database connection
#' @param store character vector either "tmp" or "RAM"
temp_location <- function(db, store = c("tmp", "RAM")){
    store <- match.arg(store)
    switch(store,
           tmp = sqldf("pragma temp_store = 1", connection = db),
           RAM = sqldf("pragma temp_store = 2", connection = db))
}

