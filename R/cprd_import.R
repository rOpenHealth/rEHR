

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

