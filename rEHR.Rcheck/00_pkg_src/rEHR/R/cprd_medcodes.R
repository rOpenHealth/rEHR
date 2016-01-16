#' Produce a dataset of CPRD medcodes with frequencies of patients in the clinical table
#' 
#' This function aggregates all distinct patients matching each CPRD medcode in the clinical table
#' 
#' Note that this does not translate to Read/OXMIS codes.
#' This function should be fast because all of the heavy lifting happens in SQLite before the data is exported to R
#' 
#' @param db a database connection
#' @param clinical_table name of the clinical table in the database
#' @param patid name of the patid field
#' @param medcode name of the medcode field
#' @export
#' @examples \dontrun{
#' medcode_counts <- patients_per_medcode(db)
#' head(medcode_counts)
#' } 
patients_per_medcode <- function(db, clinical_table = "Clinical", patid = "patid", medcode = "medcode"){
    sqldf(sprintf("SELECT %s, COUNT(DISTINCT %s) AS patients FROM %s GROUP BY %s",
                  medcode, patid, clinical_table, medcode), 
          connection = db)
}


#' Translate CPRD medcodes to Read/Oxmis
#' 
#' This function accepts a data frame with a column for CPRD medcodes and merges with a medical lookup table to give columns for Read/OXMIS codes and optional descriptions
#' 
#' Note that if the names of the medcodes columns are different in the data and the lookup table, the name in the data is retained
#' To maintain sanity, a warning will be given to inform of are any name conflicts between the input data and the lookup
#' 
#' @export
#' 
#' @param medcodes_data a dataframe with a column matching medcodes_name
#' @param lookup_table a dataframe with columns matching lookup_readcodes and lookup_medcodes
#' @param medcodes_name character name of the CPRD medcodes column in medcodes_data
#' @param lookup_readcodes character name of the Read codes column in the lookup_table
#' @param lookup_medcodes character name of the CPRD medcodes column in the lookup_table
#' @param description logical Should description and other categories from the lookup table also be included?
#' @return a data frame matching the input medcodes_data with the Read codes and optional description columns merged in.
medcodes_to_read <- function(medcodes_data, lookup_table, medcodes_name = "medcode", lookup_readcodes = "readcode", lookup_medcodes = "medcode", description = TRUE){
    if(c(lookup_readcodes, lookup_medcodes) %in% names(lookup_table) && medcodes_name %in% names(medcodes_data)){
        if(!description) lookup_table <- lookup_table[, c(lookup_medcodes, lookup_readcodes)]
        if(medcodes_name != lookup_medcodes) names(lookup_table)[names(lookup_table) == lookup_medcodes] <- medcodes_name
        if(intersect(names(lookup_table), names(medcodes_data)) != medcodes_name) warning("Name conflicts in data and lookup. output names may not be sane!")
        merge(medcodes_data, lookup_table, all.x = TRUE, by = medcodes_name)
    } else stop("Names in lookup/data do not match those specified")
}


#' Translate Read/Oxmis codes to CPRD medcodes
#' 
#' This function accepts a data frame with a column for Read/Oxmis codes and merges with a medical lookup table to give columns for CPRD medcodes and optional descriptions
#' 
#' Note that if the names of the Read/Oxmis codes columns are different in the data and the lookup table, the name in the data is retained
#' To maintain sanity, a warning will be given to inform of are any name conflicts between the input data and the lookup
#' 
#' @export
#' 
#' @param readcodes_data a dataframe with a column matching medcodes_name
#' @param lookup_table a dataframe with columns matching lookup_readcodes and lookup_medcodes
#' @param readcodes_name character name of the Read codes column in readcodes_data
#' @param lookup_readcodes character name of the Read codes column in the lookup_table
#' @param lookup_medcodes character name of the CPRD medcodes column in the lookup_table
#' @param description logical Should description and other categories from the lookup table also be included?
#' @return a data frame matching the input medcodes_data with the Read codes and optional description columns merged in.
read_to_medcodes <- function(readcodes_data, lookup_table, readcodes_name = "readcode", 
                             lookup_readcodes = "readcode", lookup_medcodes = "medcode", description){
    if(c(lookup_readcodes, lookup_medcodes) %in% names(lookup_table) && readcodes_name %in% names(readcodes_data)){
        if(!description) lookup_table <- lookup_table[, c(lookup_medcodes, lookup_readcodes)]
        if(readcodes_name != lookup_readcodes) names(lookup_table)[names(lookup_table) == lookup_readcodes] <- readcodes_name
        names_in_both <- intersect(names(lookup_table), names(readcodes_data))
        if(length(names_in_both) > 1) warning("Name conflicts in data and lookup. output names may not be sane!")
        out <- merge(readcodes_data, lookup_table, all.x = TRUE, by = readcodes_name)
        out[!is.na(out[[lookup_medcodes]]),]
    } else stop("Names in lookup/data do not match those specified")
}









