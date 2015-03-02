require(rEHR)
library(rClinicalCodes)
library(dplyr)
library(stringr)

#' This script controls the building of the simulated data sets used to demonstrate rEHR
#' The simulated dataset follows the structure of CPRD, but it is reduced to its bare essential
#' elements needed to demonstrate the functionality of the package.


## Get lists of clinical codes for QOF conditions from www.clinicalcodes.org:
QOF_BRv24 <- get_ClinicalCodes(article_id = 1)

## Prevalences of QOF conditions in the population:
condition_prevalence <- structure(c(0.4, 0.3, 0.3, 0.5, 0.4, 0.2, 0.1, 0.1, 0.1, 0.3, 
                                    0.2, 0.1, 0.05, 0.1, 0.1, 0.1, 0.1), 
                                  .Names = c("chd", "heart_failure", 
                                             "stroke", "hypertension", "diabetes", "copd", 
                                             "epilepsy", "hypothyroidism", "cancer", "asthma", 
                                             "ckd_stage3", "ckd_stage4", "ckd_stage5", 
                                             "atrial_fibrilation", "learning_disability", 
                                             "peripheral_arterial_disease", "osteoporosis"))

## Accessing CPRD lookup files (set the CPRDLOOKUPS environment variable to cprd lookups directory)
cprd_entity <- read.delim(file.path(Sys.getenv("CPRDLOOKUPS"), "Entity.txt"))
cprd_medical <- read.delim(file.path(Sys.getenv("CPRDLOOKUPS"), "medical.txt"))
cprd_product <- read.delim(file.path(Sys.getenv("CPRDLOOKUPS"), "product.txt"))
cprd_bnfcodes <- read.delim(file.path(Sys.getenv("CPRDLOOKUPS"), "bnfcodes.txt"))

QOF_BRv24 <- lapply(1:length(QOF_BRv24), function(x){
    structure(read_to_medcodes(QOF_BRv24[[x]], lookup_table = cprd_medical, 
                               readcodes_name = "code", description = FALSE),
              prevalence = condition_prevalence[x])
})
names(QOF_BRv24) <- names(condition_prevalence)



# Build sample lookup tables -------------------------------------

qof_codes <- as.character(unlist(lapply(QOF_BRv24, function(x) x$code)))
smoking_codes <- read_to_medcodes(get_ClinicalCodes(article_id = 5, codelist_name="smoking_status"), 
                                  lookup_table = cprd_medical, readcodes_name = "code", 
                                  description = FALSE)
smoking_codes %>% filter(smoking_status == 2) -> smoking_codes
hba1c_codes <- read_to_medcodes(get_ClinicalCodes("https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/5/codelist/hba1c_testing/download/"),
                                lookup_table = cprd_medical, readcodes_name = "code", 
                                description = FALSE)
suppressWarnings(qof_codes <- bind_rows(QOF_BRv24))

suppressWarnings(clinical_codes <- bind_rows(qof_codes, smoking_codes, hba1c_codes))
clinical_codes %>% mutate(list = str_extract(list_name, "[0-9a-zA-Z_]*$"),
                          readcode = code,
                          desc = description,
                          medcode = 1:nrow(clinical_codes)) %>%
    select(medcode, readcode, desc, list) -> clinical_codes



entity <- cprd_entity %>% filter(enttype %in% c(275, 163, 164, 165, 1, 13)) %>%
    select(enttype, description, filetype, category, data1, data2, data3, data4)
entity$enttype <- 1:nrow(entity)

product <- cprd_product[sample(nrow(cprd_product), 500),]
product$prodcode <- 1:nrow(product)
product <- product %>% select(-gemscriptcode)
product$productname <- iconv(product$productname, "latin1", "ASCII", sub="")
product$drugsubstance <- NULL
product$strength <- NULL
product$formulation <- NULL
product$route <- NULL


# Simulating an EHR for testing ------------------------------

betas <- log(c(1.2, 1.4, 1.3, 1.1, 1.2, 1.2, 1.4, 1.1, 
               1.7, 1.01, 1.2, 1.4, 1.6, 1.5, 1.1, 1.4, 1.4))
names(betas) <- names(condition_prevalence)


ehr_def <- define_EHR(patient = list(num = 250,
                                 comorbidity = list(
                                     codes = QOF_BRv24,
                                     prevalence =  condition_prevalence),
                                 sim_params = list(transfer_out_prob = 0.2,
                                                   scale = 25000,
                                                   weibull_shape = 1,
                                                   censor_type = "noninformative",
                                                   betas = list(conditions = betas,
                                                                gender = log(0.7),
                                                                baseline = 0.01,
                                                                transfer_out = 3.5))))

use_data(clinical_codes, overwrite = TRUE)
use_data(entity, overwrite = TRUE)
use_data(product, overwrite = TRUE)
use_data(ehr_def, overwrite = TRUE)


# build patient table -------------------------------------

ehr_patients <- simulate_ehr_patients(ehr_def)
write.table(ehr_patients, "inst/ehr_data/ehr_Patient.txt", sep = "\t")


system.file("ehr_data", "ehr_Patient.txt", package = "rEHR")


# checking the data are broadly sane...
plot(density(as.numeric(pmin(ehr_patients$tod, 
                             ehr_def$end_date, na.rm = TRUE) - ehr_patients$crd) / 365.25 ))
table(ehr_patients$toreason)
s <- Surv(as.numeric(ehr_patients$crd - as.Date(ehr_def$start_date)) / 365.25, 
          as.numeric(pmin(ehr_patients$tod, 
                          ehr_def$end_date, na.rm = TRUE) - 
                         as.Date(ehr_def$start_date)) / 365.25,
          ehr_patients$toreason == 1)
mod1 <- coxph(s ~ gender + chd + heart_failure + stroke + 
                  hypertension + diabetes + copd + epilepsy + hypothyroidism + cancer + 
                  asthma + ckd_stage3 + ckd_stage4 + ckd_stage5 + atrial_fibrilation + 
                  learning_disability + peripheral_arterial_disease + osteoporosis, 
              data = ehr_patients)
cbind(exp(c(ehr_def$betas$gender, betas)), exp(mod1$coefficients))
# reduced size for CRAN - betas are way off - set patient number to 10000 for better estimates

# build practice table -------------------------------

practice <- simulate_ehr_practices(ehr_def)
write.table(ehr_patients, "inst/ehr_data/ehr_Practice.txt", sep = "\t")


# build consultation table -----------------------------

consultation <- simulate_ehr_consultations(ehr_def, patient_table = ehr_patients, cores = 4)
write.table(consultation, "inst/ehr_data/ehr_Consultation.txt", sep = "\t")


# build Clinical events table --------------------------

clinicals <- simulate_ehr_events(ehr_def, consultation, event_type = "clinical", cores = 4)
write.table(clinicals, "inst/ehr_data/ehr_Clinical.txt", sep = "\t")

# build referral events table --------------------------

referrals <- simulate_ehr_events(ehr_def, consultation, event_type = "referral", cores = 4)
write.table(referrals, "inst/ehr_data/ehr_Referral.txt", sep = "\t")


# build therapy events table ---------------------------

therapies <- simulate_ehr_events(ehr_def, consultation, event_type = "therapy", cores = 4,
                                 therapy_lookup = product)
write.table(therapies, "inst/ehr_data/ehr_Therapy.txt", sep = "\t")
