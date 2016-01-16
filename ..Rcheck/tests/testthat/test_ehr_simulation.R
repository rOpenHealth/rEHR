context("Constuction of simulated EHR data")

patient <- simulate_ehr_patients(ehr_def)
practice <- simulate_ehr_practices(ehr_def)


test_that("ehr_def is sane", {
    expect_is(ehr_def, "EHR_definition")
    expect_is(ehr_def$start_date, "Date")
    expect_is(ehr_def$end_date, "Date")
})

test_that("Patient table is sane", {
    expect_equal(nrow(patient), ehr_def$patient$num)
    expect_true(all(c("patid", "practid", "gender", "yob", "mob", 
                      "crd", "tod", "toreason", "deathdate") %in% names(patient)))
    expect_true(all(names(ehr_def$patient$comorbidity$codes) %in% names(patient)))
    expect_true(all(!is.na(patient[, names(ehr_def$patient$comorbidity$codes)])))
    expect_is(patient$crd, "Date")
    expect_is(patient$tod, "Date")
    expect_is(patient$deathdate, "Date")
    expect_true(all(patient$yob >= as.integer(format(as.Date(ehr_def$start_date), format = "%y")) - 1800))
    expect_more_than(sum(patient[,names(ehr_def$patient$comorbidity$codes)]), 0)
    expect_equal(length(unique(patient$patid)), nrow(patient))
})

test_that("Practice table is sane", {
    expect_equal(nrow(practice), ehr_def$practice$num)
    expect_true(all(c("practid", "region", "lcd", "uts", "imd_5") %in% names(practice)))
    expect_is(practice$lcd, "Date")
    expect_is(practice$uts, "Date")
    expect_true(all(practice$lcd > practice$uts))
})

patient_def <- ehr_def$patient
reduced_patients <- 100
patient_def$num <- reduced_patients
ed2 <- define_EHR(patient = patient_def)

test_that("define_EHR is sane", {
    expect_warning(define_EHR())
    expect_named(ed2, c("start_date", "end_date", "patient", 
                                 "practice", "clinical", "referral", 
                                 "consultation", "therapy"), ignore.order = TRUE)
    expect_is(ed2, "EHR_definition")
    expect_is(ed2$start_date, "Date")
    expect_is(ed2$end_date, "Date")
    expect_equal(ed2$patient$num, reduced_patients)
})

patient2 <- simulate_ehr_patients(ed2)
practice2 <- simulate_ehr_practices(ehr_def)
consultation <- simulate_ehr_consultations(ed2, patient2, 1)

test_that("Consultation table is sane", {
    expect_true(length(unique(consultation$patid)) <= reduced_patients)
    expect_true(all(c("patid", "practid", "eventdate", "constype", "comorbidity", 
                      "consid") %in% names(consultation)))
    expect_is(consultation$eventdate, "Date")
    expect_is(consultation$patid, "integer")
    expect_is(consultation$practid, "integer")
    expect_is(consultation$consid, "integer")
})

cons_tab <- reshape2::dcast(as.data.frame(table(patid = consultation$patid, 
                                      comorbidity = consultation$comorbidity)), 
                  patid ~ comorbidity, value.var = "Freq")
comorbidity_check <- sapply(1:nrow(cons_tab), function(x){
    comorbid_names <- names(cons_tab)[2:length(cons_tab)][cons_tab[x, 2:length(cons_tab)] > 0]
    this_patient <- names(patient2)[10:length(patient2)][patient2[patient2$patid == cons_tab[x, "patid"], 
                                                                  10:length(patient2)] > 0]
    all(comorbid_names %in% this_patient)
})


test_that("Consultation comorbidities match those in the patient table", {
    expect_true(all(comorbidity_check))
})


clinicals <- simulate_ehr_events(ehr_def, consultation, event_type = "clinical", cores = 1)

test_that("Clinicals table is sane", {
    expect_true(length(unique(clinicals$patid)) <= reduced_patients)
    expect_named(clinicals, c("patid", "eventdate", "constype", "consid", "medcode", "comorbidity"))
    expect_is(clinicals$eventdate, "Date")
    expect_is(clinicals$patid, "integer")
    expect_is(clinicals$constype, "integer")
    expect_is(clinicals$consid, "integer")
    expect_is(clinicals$comorbidity, "character")
})


referrals <- simulate_ehr_events(ehr_def, consultation, event_type = "referral", cores = 1)

test_that("Referrals table is sane", {
    expect_true(length(unique(referrals$patid)) <= reduced_patients)
    expect_named(referrals, c("patid", "eventdate", "constype", "consid", "medcode", "comorbidity"))
    expect_is(referrals$eventdate, "Date")
    expect_is(referrals$patid, "integer")
    expect_is(referrals$constype, "integer")
    expect_is(referrals$consid, "integer")
    expect_is(referrals$comorbidity, "character")
})

therapies <- simulate_ehr_events(ehr_def, consultation, event_type = "therapy", cores = 1,
                                 therapy_lookup = product)


test_that("Therapy table is sane", {
    expect_true(length(unique(therapies$patid)) <= reduced_patients)
    expect_named(therapies, c("patid", "eventdate", "constype", "consid", "prodcode", "productname", 
                              "bnfcode", "bnfchapter", "comorbidity"))
    expect_is(therapies$eventdate, "Date")
    expect_is(therapies$patid, "integer")
    expect_is(therapies$constype, "integer")
    expect_is(therapies$consid, "integer")
    expect_is(therapies$prodcode, "integer")
    expect_is(therapies$productname, "character")
    expect_is(therapies$bnfcode, "factor")
    expect_is(therapies$bnfchapter, "factor")
    expect_is(therapies$comorbidity, "character")
})



