

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
#' Connects to a SQLite database or creates one if it does not already exist
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
#' @param dateformat the format that dates are stored in the CPRD data.  If this is wrong it won't break but all dates are likely to be NA
#' @param yob_origin value to add yob values to to get actual year of birth (Generally 1800)
#' @export
add_to_database <- function(db, files, table_name, dateformat = "%d/%m/%Y", yob_origin = 1800){
    date_fields <- c("eventdate", "sysdate", "lcd", "uts", "frd", "crd", "tod", "deathdate")
    for(f in files){
        if(str_detect(f, "zip$")){
            message(sprintf("Unzipping %s...", f), appendLF = FALSE)
            dat <- read_zip(f)
        } else {
            message(sprintf("Reading %s...", f), appendLF = FALSE)
            dat <- read.delim(f)
        }
        f_dates <- intersect(names(dat), date_fields)
        if(length(f_dates)){
            message(" Converting date formats...", appendLF = FALSE)
            for(column in f_dates){
                dat[[column]] <- as.character(as.Date(dat[[column]], format = dateformat))
            }
        }
        if("yob" %in% names(dat)) dat$yob <- dat$yob + yob_origin
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
#' This function may take a long time to process because it unzips (potentially large) files, reads into R where it convertsthe date formats 
#' before importing to SQLite. However, this initial data preparation step will greatly accelarate downstream processing.
#' @param db a database connection
#' @param data_dir the directory containing the CPRD cohort data
#' @param filetypes character vector of filetypes to be imported
#' @param dateformat the format that dates are stored in the CPRD data.  If this is wrong it won't break but all dates are likely to be NA
#' @param yob_origin value to add yob values to to get actual year of birth (Generally 1800)
#' @export
import_CPRD_data <- function(db, data_dir,
                             filetypes = c("Additional", "Clinical", "Consultation", 
                                           "Immunisation", "Patient", "Practice", 
                                           "Referral", "Staff", "Test", "Therapy"),
                             dateformat = "%d/%m/%Y", yob_origin = 1800){
    all_files <- list.files(data_dir)
    for(filetype in filetypes){
        current <- all_files[str_detect(all_files, paste("PET", filetype, sep = "_"))]
        current  <- file.path(data_dir, current)
        if(!length(current)){
            message(sprintf("No %s files to import.", filetype))
        } else {
            add_to_database(db, files = current, table_name = filetype, 
                            dateformat = dateformat, yob_origin = yob_origin)
        }
    }
}






