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
#' @param keyword_field character identifying the field in the lookup table to be searched 
#' for keywords
#' @return a data frame subsetted by keyword
#' @examples \dontrun{
#' keywords <- c('oral ulceration', 'mouth ulceration', 'aphthous ulceration', 
#' 'oral aphthous ulceration','oral ulcer[s]?', 'mouth ulcer[s]?', 'aphthous ulcer[s]?', 
#' 'aphthous stomatitis', "stomatitis", "aphthae", 'oral aphthous stomatitis', 
#' 'oral aphthous ulcers', 'recurrent oral ulcers', 'recurrent mouth ulcers', 
#' 'recurrent oral aphthous ulcers', 'recurrent aphthous ulcers', 'recurrent aphthous stomatitis', 
#' 'recurrent oral aphthous stomatitis')
#' a <- extract_keywords(medical, keywords)
#' }
extract_keywords <- function(lookup, keywords, keyword_field = "desc"){
    regex <- paste(keywords, collapse  = "|")
    lookup[str_detect(lookup[[keyword_field]], ignore.case(regex)),]
}


#' Constructor function for MedicalDefinition class
#' @export
#' @param terms list of character vectors or NULL
#' @param codes list of character vectors or NULL
#' @param tests list of character vectors or NULL
#' @param drugs list of character vectors or NULL
#' @param drugcodes list of character vectors or NULL
#'
#' @details
#' Elements marked with a "-" are excluded. Elements marked with a "r%" are resolved codes
#' @examples
#' def <- MedicalDefinition(terms = list(c("angina", "unstable"), c("angina", "Crescendo "),
#'                                       c("angina", "Refractory")),
#'                          codes = list("G33..00", "G330.00", "%r212H", "-G617"))
#' class(def)
MedicalDefinition <- function(terms = NULL, codes = NULL, tests = NULL,
                              drugs = NULL, drugcodes = NULL){
    def <- structure(
        list(terms = terms,
             codes = terms,
             tests = terms,
             drugs = terms,
             drugcodes = drugcodes),
        class = "MedicalDefinition")
    for(item in def){
        assert_that(is.null(item) || is.list(item))
    }
    def
}

#' Imports definitions to be searched from a csv file into a MedicalDefinition object
#' @export
#' @param input_file character path to the input file
#' @examples
#' def2 <- import_definitions(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
import_definitions <- function(input_file){
    con  <- file(input_file, open = "r")
    def_list <- MedicalDefinition(list(), list(), list(), list(), list())
    n <- 1
    
    while (length(point <- readLines(con, n = 1, warn = FALSE)) > 0) {
        point <- gsub("\\\\","\\", point) # get rid of double escape characters due to readLines
        point_data <- strsplit(point, ",")[[1]]
        if (all(point_data[1:3] == c("definition", "status", "items"))) {
            point <- readLines(con, n = 1, warn = FALSE)
            if(!length(point)) break
            point <- gsub("\\\\","\\", point) # get rid of  double escape characters
            point_data <- strsplit(point, ",")[[1]]
        }
        assert_that(point_data[1] %in% names(def_list))
        if(point_data[2] == "include"){
            def_list[[point_data[1]]][[length(def_list[[point_data[1]]]) + 1]] <- point_data[3:length(point_data)]
        } else if (point_data[2] == "resolved") {
            def_list[[point_data[1]]][[length(def_list[[point_data[1]]]) + 1]] <- paste0("r%", point_data[3:length(point_data)])
        } else if (point_data[2] == "exclude"){
            def_list[[point_data[1]]][[length(def_list[[point_data[1]]]) + 1]] <- paste0("-", point_data[3:length(point_data)])
        } else stop("You must mark the 2nd column of each row as 'include', 'exclude', or 'resolved'")
        n <- n + 1
    }
    close(con)
    def_list[vapply(def_list, function(x) length(x) == 0, TRUE)] <- list(NULL)
    def_list
}

#' Exports definition searches to an excel file
#' @export
#' @param definition_search a list of dataframes as produced by build_definition_lists
#' @param out_file file path to the excel file to be exported
#' @examples \dontrun{
#' medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
#' draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
#' out_file <- "def_searches.xlsx"
#' export_definition_search(draft_lists, out_file)
#' }
export_definition_search <- function(definition_search, out_file){
    append_p <- FALSE
    for (def in names(definition_search)){
        if(!is.null(definition_search[[def]])){
            xlsx::write.xlsx(definition_search[[def]], file = out_file,
                             sheetName = def, append = append_p)
            append_p <- TRUE
        }
    }
}

#' This function is used to build new definition lists based on medical definitions
#'
#'@details
#' You may get an invalid multibyte string error, in which case, set fileEncoding="latin1" on
#' read.delim when reading in the lookup tables
#' Lookup tables are
#' @export
#' @param def an object of class MedicalDefinition
#' @param medical_table Dataframe lookup table of clinical codes
#' @param test_table dataframe lookup table of test codes
#' @param drug_table dataframe lookup table of medication product codes
#' @param lookup list containing elements: "codes", "terms", "tests", "drugs", "drugcodes" (see details)
#' @examples \dontrun{
#' medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
#' draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
#' }
#'
definition_search <- function(def, medical_table = NULL, test_table = NULL,
                              drug_table = NULL, lookup = NULL){
    if(is.null(lookup)){
        if(exists(".ehr")){
            lookup <- .ehr$lookup
        } else {
            stop("Either library(rEHR) and choose an EHR to lookup from or assign your own lookup (See details)")
        }
        
    }
    
    ## Helper functions:
    
    ## Convert to lowercase if not all capitals, plus replace underscores with spaces
    fix_case <- function(input){
        if(is(input, "list")){
            lapply(input, fix_case)
        } else {
            ifelse(toupper(input) == input, str_replace_all(input, "_", " "),
                   tolower(str_replace_all(input, "_", " ")))
        }
    }
    ## convert character vector s to a regex AND with any word order
    regex_and <- function(s){
        paste0(lapply(combinat::permn(s),
                      function(x) paste0("(", paste0(".*?(", x, ")", collapse=""), ")")),
               collapse = "|")
    }
    ## return dummy regex if no cases
    check_terms <- function(p, out){
        if(any(p)){
            out
        } else "ZZZZZZZZZZZZZZZZZ"
    }
    
    ## Builds the regex expression for the deired terms then extracts the matching terms from
    ## the lookup table
    
    lookup_terms <- function(term_table){
        terms <- fix_case(def[[def_name]])
        excludes <- sapply(terms, function(x) substring(x, 1, 1)[1] == "-")
        exclude_terms <- check_terms(excludes, sub("^-", "", unlist(terms[excludes])))
        
        terms <- terms[!excludes]
        resolved <- sapply(terms, function(x) substring(x, 1, 2)[1] == "r%")
        resolved_terms <- check_terms(resolved, sub("^r%", "", terms[resolved]))
        
        simple_terms_p <- vapply(terms, function(x) length(x) == 1, TRUE)
        simple_terms <- check_terms(simple_terms_p,
                                    paste0("(", paste0(terms[simple_terms_p],
                                                       collapse = ")|("), ")"))
        complex_terms_p <- vapply(terms, function(x) length(x) > 1, TRUE)
        complex_terms <- check_terms(complex_terms_p,
                                     paste0(lapply(terms[complex_terms_p], regex_and),
                                            collapse = "|"))
        regex <- paste0(complex_terms, "|", simple_terms, sep = "")
        exclude_regex <- paste0("(", paste0(exclude_terms, collapse = ")|("), ")")
        if (length(lookup[[def_name]]) == 1){
            lookup_terms <- fix_case(term_table[[lookup[[def_name]]]])
        } else { # concatenate if multiple search variables
            lookup_terms <- fix_case(apply(term_table[, lookup[[def_name]]], 1,
                                           paste, sep = " ", collapse = " "))
        }
        matches <- str_detect(lookup_terms, regex) &
            !str_detect(lookup_terms, exclude_regex)
        terms_out <- filter(term_table, matches) %>%
            mutate(resolved = 0)
        terms_out$resolved[str_detect(terms_out[[lookup[[def_name]]]], resolved_terms)] <- 1
        list(terms = terms_out, excludes = exclude_terms)
    }
    
    lookup_codes <- function(term_table){
        term_table <- arrange_(term_table, lookup$codes )
        #   search.codes<-str_replace_all(search.codes,' ','')
        #   search.codes.set<-unlist(str_split(search.codes,','))
        codes <- unlist(def[[def_name]])
        range_codes <- codes[str_detect(codes, ".+(-)")]
        
        range_table_codes <- dplyr::bind_rows(lapply(range_codes, function(x){
            rang.codes <- unlist(str_split(x, '-'))
            rang.codes <- str_c('^',rang.codes)
            pos.ini <- which(str_detect(term_table$readcode, rang.codes[1]))[1]
            pos.fin <- which(str_detect(term_table$readcode, rang.codes[2]))
            pos.fin <- pos.fin[length(pos.fin)]
            term_table[pos.ini:pos.fin,]
        })) %>%
            mutate(resolved = 0)
        
        ## remove exclusions and note resolves
        single_codes <- codes[!codes %in% range_codes]
        excludes <- sapply(single_codes, function(x) substring(x, 1, 1)[1] == "-")
        exclude_codes <- check_terms(excludes, sub("^-", "", single_codes[excludes]))
        resolved <- sapply(single_codes, function(x) substring(x, 1, 2)[1] == "r%")
        resolved_codes <- paste(str_c("^", check_terms(resolved,
                                                       sub("^r%", "", single_codes[resolved]))),
                                collapse = "|")
        single_codes <- single_codes[!excludes]
        single_codes <-  sub("^r%", "", single_codes)
        code_regex <- paste0(str_c('^', single_codes), collapse = "|")
        exclude_regex <- paste0(str_c('^', exclude_codes), collapse = "|")
        lookup_terms <- term_table[[lookup[[def_name]]]]
        matches <- str_detect(lookup_terms, code_regex) &
            !str_detect(lookup_terms, exclude_regex)
        code_table <- filter(term_table, matches)
        code_table$resolved <- 0
        code_table$resolved[ str_detect(code_table$readcode, resolved_codes)] <- 1
        
        all_codes <- dplyr::bind_rows(code_table, range_table_codes) %>%
            dplyr::distinct_(lookup$codes)
        
        list(codes = all_codes, excludes = exclude_codes)
    }
    
    ## cases for the different tables to be searched
    for(def_name in names(def)){
        if (def_name == "terms"){
            if (is.null(def[[def_name]])) {
                all_terms <- NULL
                next
            }
            assert_that(!is.null(medical_table))
            all_terms <- lookup_terms(term_table = medical_table)
        } else if (def_name == "codes"){
            if (is.null(def[[def_name]])) {
                all_codes <- NULL
                next
            }
            assert_that(!is.null(medical_table))
            all_codes <- lookup_codes(term_table = medical_table)
        } else if (def_name == "drugs"){
            if (is.null(def[[def_name]])) {
                all_drugs <- NULL
                next
            }
            assert_that(!is.null(drug_table))
            all_drugs <- lookup_terms(term_table = drug_table)
        } else if (def_name == "tests"){
            if (is.null(def[[def_name]])) {
                all_tests <- NULL
                next
            }
            assert_that(!is.null(test_table))
            all_tests <- lookup_terms(term_table = test_table)
        }
    }
    ## combine terms and codes tables, removing duplicates
    if(!is.null(all_terms) && !is.null(all_codes)){
        combined_terms_codes <- dplyr::bind_rows(all_terms$terms, all_codes$codes) %>%
            dplyr::distinct_(lookup$codes)
    } else combined_terms_codes <- NULL
    list(terms_table = all_terms$terms, codes_table = all_codes$codes,
         combined_terms_codes = combined_terms_codes,
         drugs_table = all_drugs$terms, tests_table = all_tests$terms)
}

#' Basic print method for medical definition classes
#' @export
#' @param x an object of class "medical_definition"
#' @param \dots Potential further arguments (required for method/generic reasons)
print.MedicalDefinition <- function(x, ...){
    cat("Medical definition:\n")
    assert_that(all(names(x) %in% c("terms", "codes", "tests", "drugs", "drugcodes")))
    str(x, max.level = 1)
}
