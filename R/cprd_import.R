

#' Reads a zipped data file to a dataframe
#' @param file character a file to read in
#' @param ... extra arguments to pass to read.delim
#' @return a dataframe
#' @export
read_zip <- function(file, ...) {
    zipFileInfo <- unzip(file, list=TRUE)
    if(nrow(zipFileInfo) > 1)
        stop("More than one data file inside zip")
    else
        read.delim(unz(file, as.character(zipFileInfo$Name)), ...)
}


#' Wrapper for dbconnect
#' @param dbname a name for the new database
#' @export
database <- function(dbname){
    dbConnect(SQLite(), dbname = paste(dbname, "sqlite", sep = "."))
}

#' Adds a series of files to a database
#' Will automatically unzip files before calling them in
#' @param db a database connection object
#' @param files a character vector of filenames to files to be imported
#' @param table_name a name for the table to import to
#' @export
add_to_database <- function(db, files, table_name){
    for(f in files){
        if(str_detect(f, "zip$")){
            message(sprintf("Unzipping %s...", f), appendLF = FALSE)
            dat <- read_zip(f)
        } else {
            message(sprintf("Reading %s...", f), appendLF = FALSE)
            dat <- read.delim(f)
        }
        message(sprintf(" Importing to table '%s'...", table_name))
        if(dbExistsTable(db, table_name)){
            dbWriteTable(conn = db, 
                         name = table_name, 
                         value = dat, 
                         row.names = FALSE,
                         append = TRUE)
        } else dbWriteTable(conn = db, 
                            name = table_name, 
                            value = dat, 
                            row.names = FALSE)
    }
}

#' Imports all selected CPRD data into an sqlite database
#' Note that if you chose to import all the filetype, you may end up with aa very large database file.
#' You may then chose only to import the files you want to use.  You can always import the rest of the files later.
#' @param db a database connection
#' @param data_dir the directory containing the CPRD cohort data
#' @param filetypes character vector of filetypes to be imported
#' @export
import_CPRD_data <- function(db, data_dir,
                             filetypes = c("Additional", "Clinical", "Consultation", 
                                           "Immunisation", "Patient", "Practice", 
                                           "Referral", "Staff", "Test", "Therapy")){
    all_files <- list.files(data_dir)
    for(filetype in filetypes){
        current <- all_files[str_detect(all_files, paste("PET", filetype, sep = "_"))]
        current  <- file.path(data_dir, current)
        if(!length(current)){
            message(sprintf("No %s files to import.", filetype))
        } else {
            add_to_database(db, files = current, table_name = filetype)
        }
    }
}






