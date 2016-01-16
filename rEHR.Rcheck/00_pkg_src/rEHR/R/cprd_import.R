#' Reads a zipped data file to a dataframe
#' 
#' This function will unzip a zipped text file and read it in to an R data frame
#' 
#' Default behaviour is to read in as a standard read.delim call.
#' extra arguments to read.delim can be passed to the function
#' 
#' @export
#' 
#' @param file character a file to read in
#' @param \dots extra arguments to pass to read.delim
#' @return a dataframe
read_zip <- function(file, ...) {
    zipFileInfo <- unzip(file, list=TRUE)
    if(nrow(zipFileInfo) > 1)
        stop("More than one data file inside zip")
    else
        read.delim(unz(file, as.character(zipFileInfo$Name)), ...)
}



#' Wrapper for dbConnect
#' 
#' Connects to a SQLite database or creates one if it does not already exist
#' 
#' If the '.sqlite' file extension is ommited from the dbname argument it is automatically added.
#'
#' @export
#' 
#' @param dbname character name path to database file
#' @return SQLiteConnection object
#' @examples \dontrun{
#' db <- database("mydb")
#' }
database <- function(dbname){
    if(!str_detect(dbname, "\\.sqlite$")) {
        dbname <- paste(dbname, "sqlite", sep = ".")
    } 
    dbConnect(SQLite(), dbname)
}


#' Adds a series of files to a database
#' 
#' This function can be used to import a CPRD file or files into a SQLite database connection.
#' 
#' Will automatically unzip files before calling them in
#' If practid is TRUE, a practid variable is constructed by converting the last 3 digits of the patient id (if supplied) to a numeric.
#' If filenames is TRUE, source data filenames are included as a variable with the filetypes stripped away.
#' 
#' @export
#'  
#' @param db a database connection object
#' @param files a character vector of filenames to files to be imported
#' @param table_name a name for the table to import to
#' @param dateformat the format that dates are stored in the CPRD data.  If this is wrong it won't break but all dates are likely to be NA
#' @param yob_origin value to add yob values to to get actual year of birth (Generally 1800)
#' @param practid logical should practice id variable be constructed from the patient ids?
#' @param filenames logical should the filename be included as a variable?
add_to_database <- function(db, files, table_name, dateformat = "%d/%m/%Y", yob_origin = 1800, practid = TRUE, filenames = FALSE){
    date_fields <- c("eventdate", "sysdate", "lcd", "uts", "frd", "crd", "tod", "deathdate")
    for(f in files){
        if(str_detect(f, "zip$")){
            message(sprintf("Unzipping %s...", f), appendLF = FALSE)
            dat <- read_zip(f, stringsAsFactors = FALSE)
        } else {
            message(sprintf("Reading %s...", f), appendLF = FALSE)
            dat <- read.delim(f, stringsAsFactors = FALSE)
        }
        f_dates <- intersect(names(dat), date_fields)
        if(length(f_dates)){
            message(" Converting date formats...", appendLF = FALSE)
            for(column in f_dates){
                dat[[column]] <- as.character(as.Date(as.character(dat[[column]]), 
                                                      format = dateformat))
            }
        }
        if("yob" %in% names(dat)) dat$yob <- dat$yob + yob_origin
        if(practid && "patid" %in% names(dat)){
            message(" Adding practid variable...", appendLF = FALSE)
            dat$practid <- as.integer(str_extract(dat$patid, "[0-9]{3}$"))
        }
        if(filenames) dat$filename <- str_replace(basename(f), "\\..*", "")
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
#' 
#' This function can import from both cohorts downloaded via the CPRD online tool and CPRD GOLD builds
#' 
#' Note that if you chose to import all the filetype, you may end up with a very large database file.
#' You may then chose only to import the files you want to use.  You can always import the rest of the files later.
#' This function may take a long time to process because it unzips (potentially large) files, reads into R where it converts the date formats 
#' before importing to SQLite. However, this initial data preparation step will greatly accelerate downstream processing.
#'
#' @export
#'  
#' @param db a database connection
#' @param data_dir the directory containing the CPRD cohort data
#' @param filetypes character vector of filetypes to be imported
#' @param dateformat the format that dates are stored in the CPRD data.  If this is wrong it won't break but all dates are likely to be NA
#' @param yob_origin value to add yob values to to get actual year of birth (Generally 1800)
#' @param regex character regular expression to identify data files in the directory. This is separated from the filetype by an underscore. e.g. 'p[0-9]{3}' in CPRD GOLD  
#' @param recursive logical should files be searched for recursively under the data_dir?
#' @param \dots arguments to be passed to add_to_database
import_CPRD_data <- function(db, data_dir,
                             filetypes = c("Additional", "Clinical", "Consultation", 
                                           "Immunisation", "Patient", "Practice", 
                                           "Referral", "Staff", "Test", "Therapy"),
                             dateformat = "%d/%m/%Y", 
                             yob_origin = 1800,
                             regex = "PET",
                             recursive = TRUE, ...){
    all_files <- list.files(data_dir, recursive = recursive)
    for(filetype in filetypes){
        current <- all_files[str_detect(all_files, paste(regex, filetype, sep = "_"))]
        current  <- file.path(data_dir, current)
        if(!length(current)){
            message(sprintf("No %s files to import.", filetype))
        } else {
            add_to_database(db, files = current, table_name = filetype, 
                            dateformat = dateformat, yob_origin = yob_origin, ...)
        }
    }
}






