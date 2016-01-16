## ----global options, include = FALSE,------------------------------------
library(devtools)
load_all("~/rOpenHealth//rEHR")

## ----, eval = TRUE, message = FALSE, warning = FALSE---------------------
if(! "rEHR" %in% rownames(installed.packages())) install.packages("rEHR")
library(rEHR)

## ----, eval = FALSE------------------------------------------------------
#  library(devtools)
#  install_github("rOpenHealth/rEHR")
#  library(rEHR)

## ----pseudocode_algo, eval = FALSE---------------------------------------
#  # Pseudocode prevalent cases algorithm
#  define a list of clinical codes for the condition
#  for each practice:
#      load clinical events files (clinical, referral, drugs etc.)
#      select clinical events matching the clinical code list
#      load patient and practice files
#      for each year:
#          select active patients
#          select events in year
#          merge active patients and events in year according to condition algorithm
#      combine all years in practice
#  combine patients in all practices
#  

## ----, message = FALSE---------------------------------------------------
## Use the simulated ehr files supplied with the package to build our database
ehr_path <- dirname(system.file("ehr_data", "ehr_Clinical.txt", package = "rEHR"))
## create a new database connection to a temporary file
db <- database(tempfile(fileext = ".sqlite"))
## Import multiple data files into the database
import_CPRD_data(db, data_dir = ehr_path,
                 filetypes = c("Clinical", "Consultation", 
                               "Patient", "Practice", 
                               "Referral"),
                 dateformat = "%Y-%m-%d", 
                 yob_origin = 1800,
                 regex = "ehr",
                 recursive = TRUE)
## Individual files can also be added:
add_to_database(db, files = system.file("ehr_data", "ehr_Therapy.txt", package = "rEHR"), 
                table_name = "Therapy", dateformat = "%Y-%m-%d")
## Use the overloaded `head` function to view a list of 
## tables or the head of individual tables:
head(db)
head(db, table = "Clinical")

## ----, message = FALSE---------------------------------------------------
diabetes_codes <- clinical_codes[clinical_codes$list == "Diabetes",]
select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"), 
              where = "medcode %in% .(diabetes_codes$medcode) & 
                       eventdate < '2006-01-01' & eventdate >= '2005-01-01'")

## ------------------------------------------------------------------------
Asthma_codes <- clinical_codes[clinical_codes$list == "Asthma",]
q <- select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"), 
              where = "medcode %in% .(Asthma_codes$medcode)", 
              sql_only = TRUE)
temp_table(db, tab_name = "Asthma", select_query = q)
head(db, temp = TRUE)
head(db, table = "Asthma")

## ----, message = FALSE---------------------------------------------------
sqldf("SELECT patid, practid, gender, yob, deathdate from Patient WHERE 
          deathdate IS NOT NULL LIMIT 6", 
      connection = db)

## ------------------------------------------------------------------------
medcodes1 <- 1:5
practice <- 255
expand_string("SELECT * FROM clinical WHERE practid == .(practice)")
wrap_sql_query("SELECT * FROM clinical WHERE practid == #1 AND medcodes in #2", 
               practice, medcodes1)

## ------------------------------------------------------------------------
first_DM <- first_events(db, tab = "Clinical", 
                         columns = c("patid", "eventdate", "medcode"), 
              where = "medcode %in% .(diabetes_codes$medcode)")
last_DM <- last_events(db, tab = "Clinical", 
                       columns = c("patid", "eventdate", "medcode"), 
              where = "medcode %in% .(diabetes_codes$medcode)")
head(first_DM)
head(last_DM)

## ----prevalence----------------------------------------------------------
# Select all patients with current registration date (crd) < the start date 
# for each year.
registered_patients <- select_by_year(db = db, 
                         tables = "patient", 
                         columns = c("patid", "practid", "gender", 
                                     "yob", "crd", "tod", "deathdate"), 
                         where = "crd < STARTDATE",
                         year_range = c(2008:2012), 
                         year_fn = standard_years)
str(registered_patients)
table(registered_patients$year)

## ------------------------------------------------------------------------
incident_cases <- select_by_year(db = db, 
                                      tables = c("Clinical", "Referral"), 
                                      columns = c("patid", "eventdate", "medcode"), 
                                      where = "medcode %in% .(diabetes_codes$medcode) & 
                                               eventdate <= ENDDATE",
                                      year_range = c(2008:2012), 
                                      year_fn = standard_years, 
                                      selector_fn = first_events)
str(incident_cases)

## ----, message = FALSE---------------------------------------------------
## Remove duplicates across clinical and referral tables:
incident_cases %>%
    group_by(patid, year) %>%
    arrange(eventdate) %>%
    distinct() %>%
    ungroup -> incident_cases
## All patients are kept (equivalent to merge(all.x = TRUE))
prevalence_dat <- left_join(registered_patients, incident_cases)

## ----, message = FALSE---------------------------------------------------
prevalence_dat <- prev_terms(prevalence_dat)
totals <- prev_totals(prevalence_dat)
totals$prevalence$year_counts
totals$incidence$year_counts

## ----, message = FALSE---------------------------------------------------
practices <- select_events(db = db, tab = "Practice", convert_dates = TRUE)
prevalence_dat <- left_join(prevalence_dat, practices)

cohort <- build_cohort(prevalence_dat, cohort_type = "prev", 
                       cohort_start = "2006-01-01", cohort_end = "2012-12-31", 
                       diagnosis_start = "eventdate")

## ----, message = FALSE---------------------------------------------------
## Add a logical column for death during cohort
cohort$death <- with(cohort, 
                     ifelse(!is.null(deathdate) & 
                                (deathdate > as.Date("2006-01-01") & 
                                     deathdate < as.Date("2012-12-31")), 
                            1, 0))
cohort$death[is.na(cohort$death)] <- 0

library(survival)
surv_obj <- with(cohort, Surv(start, end, death))
coxph(surv_obj ~ gender + case, data = cohort)

## ----, message  = FALSE--------------------------------------------------
cohort2 <- build_cohort(prevalence_dat, cohort_type = "incid", 
                        cohort_start = "2006-01-01", cohort_end = "2012-12-31", 
                        diagnosis_start = "eventdate")
IDM_controls <- get_matches(cases = filter(cohort2, case == 1), 
                            control_pool = filter(cohort2, case == 0), 
                            match_vars = c("gender", "region"),
                            n_controls = 4, cores = 2, 
                            method = "incidence_density", diagnosis_date  = "eventdate")

## ----, message = FALSE---------------------------------------------------

IDM_controls2 <- get_matches(cases = filter(cohort2, case == 1), 
                             control_pool = filter(cohort2, case == 0), 
                             match_vars = c("gender", "region"),
                             extra_conditions = "yob >= ( .(CASE$yob) - 2) & 
                             yob <= ( .(CASE$yob) + 2)",
                             n_controls = 4, cores = 2, 
                             method = "incidence_density", diagnosis_date  = "eventdate")

## ----, message  = FALSE, results = 'hide'--------------------------------
exact_controls3 <- get_matches(cases = filter(cohort2, case == 1), 
                            control_pool = filter(cohort2, case == 0), 
                            match_vars = c("gender", "region"),
                            n_controls = 4, cores = 2, 
                            method = "exact", diagnosis_date  = "eventdate")

## ----, message = FALSE, results = 'hide'---------------------------------
consultation_dir <- "~/R/rEHR_testing"
flat_files(db, out_dir = consultation_dir, file_type = "csv")
index_controls <- match_on_index(cases = filter(cohort2, case == 1), 
                                 control_pool = filter(cohort2, case == 0),
                                 index_var = "eventdate", 
                                 match_vars = c("gender", "region"),
                                 index_diff_limit = 90, 
                                 consult_path = consultation_dir,
                                 n_controls = 4,
                                 import_fn = function(x) convert_dates(read.csv(x)))
unlink(consultation_dir, recursive = TRUE) # clean up constructed dirs after analysis

## ------------------------------------------------------------------------
tv_test <- data.frame(id = 1:5, start = rep(0, 5), end = c(1000, 689, 1000, 874, 777), 
                   event = c(0,1,0,1,1), drug_1 = c(NA, NA, NA, 340, 460),
                   drug_2 = c(NA, 234, 554, 123, NA), 
                   drug_3_start = c(110, 110,111, 109, 110),
                   drug_3_stop = c(400, 400, 400, 400, 400),
                   stage_1 = c(300, NA, NA, NA, NA),
                      stage_2 = c(450, NA, NA, NA, NA))

## Multiple binary chronic covariates:
tv_out1 <- cut_tv(tv_test, 
                  entry = start, 
                  exit =  end, 
                  cut_var = drug_1, 
                  id_var = id, 
                  tv_name = drug_1_state)
tv_out1 <- cut_tv(tv_out1, start, end, drug_2, id_var = id, drug_2_state)
head(tv_out1)
## Binary covariates:
tv_out3 <- cut_tv(tv_test, start, end, drug_3_start, id_var = id, drug_3_state)
tv_out3 <- cut_tv(tv_out3, start, end, drug_3_stop, id_var = id, drug_3_state)
head(tv_out3)
## incremental covariates:
inc_1 <- cut_tv(tv_test, start, end, stage_1, id_var = id, disease_stage, 
                on_existing = "inc")
inc_1 <- cut_tv(inc_1, start, end, stage_2, id_var = id, disease_stage, 
                on_existing = "inc")
head(inc_1)
## Chaining combinations of the above using %>%
library(dplyr)
tv_test %>%
    cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>% 
    cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
    cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
    cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
    cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc") %>%
    cut_tv(start, end, stage_2, id_var = id, disease_stage, on_existing = "inc") %>%
    head


## ------------------------------------------------------------------------
## Example construction of a clinical code list
def <- MedicalDefinition(
    terms = list(
        "peripheral vascular disease", "peripheral gangrene", "-wrong answer",
        "intermittent claudication", "thromboangiitis obliterans",
        "thromboangiitis obliterans", "diabetic peripheral angiopathy",
        c("diabetes", "peripheral angiopathy"),  # single AND expression
        c("buerger",  "disease presenile_gangrene"),
            "-excepted", # exclusion
    codes = list("G73"),
    tests = NULL,
    drugs = list("insulin", "diabet", "aspirin")))

## ----eval = FALSE--------------------------------------------------------
#  
#  ## Use fileEncoding="latin1" to avoid any issues with non-ascii characters
#  medical_table <- read.delim("Lookups/medical.txt", fileEncoding = "latin1", stringsAsFactors = FALSE)
#  drug_table <- read.delim("Lookups/product.txt", fileEncoding = "latin1", stringsAsFactors = FALSE)
#  
#  draft_lists <- build_definition_lists(def, medical_table = medical_table, drug_table = drug_table)

## ------------------------------------------------------------------------
list_EHR_attributes()

## ------------------------------------------------------------------------
get_EHR_attribute(patient_id) # gives the attribute for patient ids
get_EHR_attribute(date_fields) # fields in the database stored as dates
get_EHR_attribute(cohort) # variables used in cohort construction

## ------------------------------------------------------------------------
set_EHR_attribute(patient_id, value = "PATIENT") # set the patient id attribute
get_EHR_attribute(patient_id)

## ------------------------------------------------------------------------
set_CPRD()
get_EHR_attribute(patient_id)

