context("Importing data from text files to db")

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

test_that("db exists and has imported tables", {
    expect_is(db, "SQLiteConnection")
    expect_equal(head(db)$tbl_name,c("Clinical", "Consultation", 
                                     "Patient", "Practice", 
                                     "Referral"))
    for(i in c("Clinical", "Consultation", 
               "Patient", "Practice", 
               "Referral")){
        expect_is(head(db, table = i), "data.frame")    
    }
})

context("Accessing data")

q <- select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"), 
                   where = "medcode %in% .(1090:1122)", 
                   sql_only = FALSE)


test_that("select_events is sane", {
    expect_is(q, "data.frame")
    expect_named(q, c("patid", "eventdate", "medcode"))
    expect_true(all(q$medcode %in% 1090:1122))
})

q1 <- select_events(db, tab = "Clinical", columns = c("patid", "eventdate", "medcode"), 
                   where = "medcode %in% .(1090:1122)", 
                   sql_only = TRUE)
temp_table(db, tab_name = "Asthma", select_query = q1)
head(db, table = "Asthma")

test_that("temp_tables can be created", {
    expect_is(head(db, temp = TRUE), "data.frame")
    expect_true(nrow(head(db, temp = TRUE)) == 1)
    expect_is(head(db, table = "Asthma"), "data.frame")
    expect_named(head(db, table = "Asthma"), c("patid", "eventdate", "medcode"))
    expect_true(all(head(db, table = "Asthma")$medcode %in% 1090:1122))
    
})










