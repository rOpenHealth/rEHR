context("Importing data from csv")

input_file <- system.file("extdata", "example_search.csv", package = "rpcdsearch")
def <- import_definitions(input_file)


test_that("import is sane", {
    expect_true(length(input_file) > 0)
    expect_is(def, "MedicalDefinition")
    expect_named(def, c("terms", "codes", "tests", "drugs", "drugcodes"))
})

