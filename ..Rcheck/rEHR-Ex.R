pkgname <- "rEHR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rEHR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("MedicalDefinition")
### * MedicalDefinition

flush(stderr()); flush(stdout())

### Name: MedicalDefinition
### Title: Constructor function for MedicalDefinition class
### Aliases: MedicalDefinition

### ** Examples

def <- MedicalDefinition(terms = list(c("angina", "unstable"), c("angina", "Crescendo "),
                                      c("angina", "Refractory")),
                         codes = list("G33..00", "G330.00", "%r212H", "-G617"))
class(def)



cleanEx()
nameEx("cut_tv")
### * cut_tv

flush(stderr()); flush(stdout())

### Name: cut_tv
### Title: cut_tv - Cuts a survival dataset on a time varying variable
### Aliases: cut_tv

### ** Examples

# A simple example dataset to be cut
tv_test <- data.frame(id = 1:5, start = rep(0, 5), end = c(1000, 689, 1000, 874, 777),
                      event = c(0,1,0,1,1), drug_1 = c(NA, NA, NA, 340, 460),
                      drug_2 = c(NA, 234, 554, 123, NA),
                      drug_3_start = c(110, 110,111, 109, 110),
                      drug_3_stop = c(400, 400, 400, 400, 400),
                      stage_1 = c(300, NA, NA, NA, NA),
                      stage_2 = c(450, NA, NA, NA, NA))

# Binary chronic covariates:
tv_out1 <- cut_tv(tv_test, start, end, drug_1, id_var = id, drug_1_state)
tv_out1 <- cut_tv(tv_out1, start, end, drug_2, id_var = id, drug_2_state)
# Binary covariates:
tv_out3 <- cut_tv(tv_test, start, end, drug_3_start, id_var = id, drug_3_state)
tv_out3 <- cut_tv(tv_out3, start, end, drug_3_stop, id_var = id, drug_3_state)
# incremental covariates:
inc_1 <- cut_tv(tv_test, start, end, stage_1, id_var = id, disease_stage, on_existing = "inc")
inc_1 <- cut_tv(inc_1, start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
# Chaining combinations of the above
## Not run: 
##D library(dplyr)
##D tv_all <- tv_test %>%
##D           cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>%
##D           cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
##D           cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
##D           cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
##D           cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc") %>%
##D           cut_tv(start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
## End(Not run)



cleanEx()
nameEx("database")
### * database

flush(stderr()); flush(stdout())

### Name: database
### Title: Wrapper for dbConnect
### Aliases: database

### ** Examples

## Not run: 
##D db <- database("mydb")
## End(Not run)



cleanEx()
nameEx("definition_search")
### * definition_search

flush(stderr()); flush(stdout())

### Name: definition_search
### Title: This function is used to build new definition lists based on
###   medical definitions
### Aliases: definition_search

### ** Examples

## Not run: 
##D medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
##D drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
##D def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
##D                                             package = "rpcdsearch"))
##D draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
## End(Not run)



cleanEx()
nameEx("expand_string")
### * expand_string

flush(stderr()); flush(stdout())

### Name: expand_string
### Title: Reads strings and expands sections wrapped in dotted parentheses
### Aliases: expand_string

### ** Examples

a <- runif(10)
 expand_string("The r code is .(a)")



cleanEx()
nameEx("export_definition_search")
### * export_definition_search

flush(stderr()); flush(stdout())

### Name: export_definition_search
### Title: Exports definition searches to an excel file
### Aliases: export_definition_search

### ** Examples

## Not run: 
##D medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
##D drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
##D def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
##D                                             package = "rpcdsearch"))
##D draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
##D out_file <- "def_searches.xlsx"
##D export_definition_search(draft_lists, out_file)
## End(Not run)



cleanEx()
nameEx("extract_keywords")
### * extract_keywords

flush(stderr()); flush(stdout())

### Name: extract_keywords
### Title: Function to extract rows from a lookup table based on keywords
### Aliases: extract_keywords

### ** Examples

## Not run: 
##D keywords <- c('oral ulceration', 'mouth ulceration', 'aphthous ulceration',
##D 'oral aphthous ulceration','oral ulcer[s]?', 'mouth ulcer[s]?', 'aphthous ulcer[s]?',
##D 'aphthous stomatitis', "stomatitis", "aphthae", 'oral aphthous stomatitis',
##D 'oral aphthous ulcers', 'recurrent oral ulcers', 'recurrent mouth ulcers',
##D 'recurrent oral aphthous ulcers', 'recurrent aphthous ulcers', 'recurrent aphthous stomatitis',
##D 'recurrent oral aphthous stomatitis')
##D a <- extract_keywords(medical, keywords)
## End(Not run)



cleanEx()
nameEx("first_events")
### * first_events

flush(stderr()); flush(stdout())

### Name: first_events
### Title: Selects the earliest event grouped by patient
### Aliases: first_events

### ** Examples

## Not run: 
##D b1 <- first_events(db, tab = "Clinical", columns = c("eventdate", "medcode"),
##D where = "medcode %in% .(a$medcode)")
##D first_events(tab = "Clinical", columns = c("eventdate", "medcode"),
##D where = "medcode %in% c(1, 2, 3, 4)", sql_only = TRUE)
## End(Not run)



cleanEx()
nameEx("get_EHR_attribute")
### * get_EHR_attribute

flush(stderr()); flush(stdout())

### Name: get_EHR_attribute
### Title: Return the value of an attribute in the .ehr environment
### Aliases: get_EHR_attribute

### ** Examples

{
set_CPRD()
get_EHR_attribute()
get_EHR_attribute(patient_id)
}



cleanEx()
nameEx("import_definitions")
### * import_definitions

flush(stderr()); flush(stdout())

### Name: import_definitions
### Title: Imports definitions to be searched from a csv file into a
###   MedicalDefinition object
### Aliases: import_definitions

### ** Examples

def2 <- import_definitions(system.file("extdata", "example_search.csv",
                                            package = "rpcdsearch"))



cleanEx()
nameEx("last_events")
### * last_events

flush(stderr()); flush(stdout())

### Name: last_events
### Title: Selects the earliest event grouped by patient
### Aliases: last_events

### ** Examples

## Not run: 
##D b2 <- last_events(db, tab = "Clinical", other_columns = c("eventdate", "medcode"),
##D where = "medcode %in% .(a$medcode)")
## End(Not run)



cleanEx()
nameEx("patients_per_medcode")
### * patients_per_medcode

flush(stderr()); flush(stdout())

### Name: patients_per_medcode
### Title: Produce a dataset of CPRD medcodes with frequencies of patients
###   in the clinical table
### Aliases: patients_per_medcode

### ** Examples

## Not run: 
##D medcode_counts <- patients_per_medcode(db)
##D head(medcode_counts)
## End(Not run)



cleanEx()
nameEx("select_by_year")
### * select_by_year

flush(stderr()); flush(stdout())

### Name: select_by_year
### Title: Runs a series of selects over a year range and collects in a
###   list of dataframes
### Aliases: select_by_year

### ** Examples

## Not run: 
##D # Output from a single table
##D where_q <- "crd < STARTDATE & (is.null(tod) | tod > ENDDATE) & accept == 1"
##D ayears <- select_by_year(db, "Patient", columns = c("patid", "yob", "tod"),
##D                          where = where_q, year_range = 2000:2003)
##D # Output from multiple tables
##D load("data/medical.RData")
##D a <- read.csv("data/chronic-renal-disease.csv")
##D a <- read_to_medcodes(a, medical, "code", lookup_readcodes= "readcode",
##D                       lookup_medcodes="medcode", description = T)
##D where_q <- "eventdate >= STARTDATE & eventdate <= ENDDATE & medcode %in% .(a$medcode)"
##D byears <- byears <- select_by_year("~/rOpenHealth/CPRD_test/Coupland/Coupland",
##D                                    c("Clinical", "Referral"),
##D columns = c("patid", "eventdate", "medcode"),
##D where = where_q, year_range = 2000:2003, as_list = FALSE,
##D cores = 10)
## End(Not run)



cleanEx()
nameEx("select_events")
### * select_events

flush(stderr()); flush(stdout())

### Name: select_events
### Title: Extracts From the database
### Aliases: select_events

### ** Examples

## Not run: 
##D # medical lookup tables are provided with CPRD
##D load("data/medical.RData")
##D a <- read.csv("data/chronic-renal-disease.csv")
##D a <- read_to_medcodes(a, medical, "code", lookup_readcodes= "readcode",
##D lookup_medcodes="medcode", description = T)
##D b <- select_events(db, tab = "Referral", columns = c("patid", "eventdate", "medcode"),
##D where = "medcode %in% .(a$medcode) & eventdate < '2000-01-01'")
##D b1 <- select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"),
##D where = "medcode %in% .(a$medcode) & eventdate < '2000-01-01'")
## End(Not run)



cleanEx()
nameEx("set_EHR_attribute")
### * set_EHR_attribute

flush(stderr()); flush(stdout())

### Name: set_EHR_attribute
### Title: Sets the value of an attribute in the .ehr environment
### Aliases: set_EHR_attribute

### ** Examples

set_CPRD()
set_EHR_attribute(practice_id, "pracid")



cleanEx()
nameEx("simulate_ehr_consultations")
### * simulate_ehr_consultations

flush(stderr()); flush(stdout())

### Name: simulate_ehr_consultations
### Title: Generates simulated GP consultation tables.
### Aliases: simulate_ehr_consultations

### ** Examples

## Not run: 
##D  patient <- simulate_ehr_patients(ehr_definition) cons <-
##D   simulate_ehr_consultations(ehr_def, patient_table = patient, cores = 4) 
## End(Not run)



cleanEx()
nameEx("temp_table")
### * temp_table

flush(stderr()); flush(stdout())

### Name: temp_table
### Title: Creates a temporary table in the database
### Aliases: temp_table

### ** Examples

## Not run: 
##D db <- database("myCPRDdb")
##D temp_table(db, tab_name = "post_2005",
##D            select_query = select_events(db, tab = "Referral",
##D                                         columns = c("patid", "eventdate", "medcode"),
##D                                         where = "eventdate > '2005-01-01'",
##D                                         sql_only = TRUE))
## End(Not run)



cleanEx()
nameEx("wrap_sql_query")
### * wrap_sql_query

flush(stderr()); flush(stdout())

### Name: wrap_sql_query
### Title: combines strings and vectors in a sensible way for select
###   queries
### Aliases: wrap_sql_query

### ** Examples

medcodes1 <- 1:5
practice <- 255
wrap_sql_query("eventdate >= STARTDATE & eventdate <= ENDDATE & medcode %in% #1 &
   practice == #2", medcodes1, practice)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
