#' Function to extract rows from a lookup table based on keywords
#' 
#' This function can be used to build draft clinical code lists based on a clinical or 
#' product lookup table and a set of keywords.
#' 
#' See www.clinicalcodes.org for clinical code lists that have been used in previous studies
#' 
#' All keywords are collapsed together in an OR statement
#' 
#' @export
#' 
#' @param lookup a dataframe containing a lookup table
#' @param keywords character vector containing the keyword terms to search for
#' @param keyword_field character identifying the field in the lookup table to be searched for keywords
#' @return a data frame subsetted by keyword
#' @examples \dontrun{
#' keywords <- c('oral ulceration', 'mouth ulceration', 'aphthous ulceration', 'oral aphthous ulceration',
#' 'oral ulcer[s]?', 'mouth ulcer[s]?', 'aphthous ulcer[s]?', 'aphthous stomatitis', "stomatitis", "aphthae", 
#' 'oral aphthous stomatitis', 'oral aphthous ulcers', 'recurrent oral ulcers', 
#' 'recurrent mouth ulcers', 'recurrent oral aphthous ulcers', 'recurrent aphthous ulcers', 
#' 'recurrent aphthous stomatitis', 'recurrent oral aphthous stomatitis')
#' a <- extract_keywords(medical, keywords)
#' }
extract_keywords <- function(lookup, keywords, keyword_field = "desc"){
    regex <- paste(keywords, collapse  = "|")
    lookup[str_detect(lookup[[keyword_field]], ignore.case(regex)),]
}
