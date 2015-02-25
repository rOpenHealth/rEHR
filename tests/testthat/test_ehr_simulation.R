context("Constuction of simulated EHR data")

patient <- simulate_ehr_patients(ehr_definition)
practice <- simulate_ehr_practices(ehr_definition)

test_that("ehr_definition is sane", {
    expect_is(ehr_definition, "EHR_definition")
})

test_that("Patient table is sane", {
    expect_equal(nrow(patient), ehr_definition$patient$num)
    expect_true(all(c("patid", "practid", "gender", "yob", "mob", 
                      "crd", "tod", "toreason", "deathdate") %in% names(patient)))
    expect_true(all(names(ehr_definition$patient$comorbidity$codes) %in% names(patient)))
    expect_true(all(!is.na(patient[, names(ehr_definition$patient$comorbidity$codes)])))
    expect_true(is.date(patient$crd))
    expect_true(is.date(patient$tod))
    expect_true(is.date(patient$deathdate))
    expect_true(all(patient$yob >= as.integer(format(as.Date(ehr_definition$start_date), format = "%y")) - 1800))
})

test_that("Practice table is sane", {
    expect_equal(nrow(practice), ehr_definition$practice$num)
    expect_true(all(c("practid", "region", "lcd", "uts", "imd_5") %in% names(practice)))
    expect_true(is.date(practice$lcd))
    expect_true(is.date(practice$uts))
    expect_true(all(practice$lcd > practice$uts))
})

