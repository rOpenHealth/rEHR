##' head for \code{SQLiteConnection} object
##' 
##' If just a database connection is selected, returns a dataframe of table names
##' If a table name is also supplied, the first n rows from this table are output
##' @export 
##' 
##' @method head SQLiteConnection
##'
##' @param x A \code{SQLiteConnection} object
##' @param table character specifying a table
##' @param n integer: Number of rows to output
##' @param temp logical should the function list the temp tables
##' @param ... Additional arguments
##'
head.SQLiteConnection <- function(x, table = NULL, n = 6L, temp = FALSE, ...){
    if(is.null(table)){
        if(temp){
            dbGetQuery(x, "SELECT type, name, tbl_name FROM sqlite_temp_master;", ...)
        } else dbGetQuery(x, "SELECT type, name, tbl_name FROM sqlite_master;", ...)
        
    } else {
        dbGetQuery(x, sprintf("SELECT * FROM %s LIMIT %d;", table, n), ...)
    }
}