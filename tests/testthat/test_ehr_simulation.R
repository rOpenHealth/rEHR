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
    expect_is(ed2, "EHR_definition")
    expect_is(ed2$start_date, "Date")
    expect_is(ed2$end_date, "Date")
    expect_equal(ed2$patient$num, reduced_patients)
})

patient2 <- simulate_ehr_patients(ed2)
practice2 <- simulate_ehr_practices(ehr_def)
consultation <- simulate_ehr_consultations(ed2, patient2, 1)

test_that("Consultation table is sane", {
    expect_less_than(length(unique(consultation$patid)), reduced_patients)
    expect_true(all(c("patid", "practid", "eventdate", "constype", "comorbidity", 
                      "consid") %in% names(consultation)))
    expect_is(consultation$eventdate, "Date")
    expect_is(consultation$patid, "integer")
    expect_is(consultation$practid, "integer")
    expect_is(consultation$consid, "integer")
})

