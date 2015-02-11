context("Time-varying covariate cutting")

tv_test <- data.frame(id = 1:5, start = rep(0, 5), end = c(1000, 689, 1000, 874, 777), 
                      event = c(0,1,0,1,1), drug_1 = c(NA, NA, NA, 340, 460),
                      drug_2 = c(NA, 234, 554, 123, NA), 
                      drug_3_start = c(110, 110,111, 109, 110),
                      drug_3_stop = c(400, 400, 400, 400, 400),
                      stage_1 = c(300, NA, NA, NA, NA),
                      stage_2 = c(450, NA, NA, NA, NA))

test_that("independent tv-covariates cut correctly", {
    tv_out1 <- cut_tv(tv_test, start, end, drug_1, id_var = id, drug_1_state)
    tv_out2 <- cut_tv(tv_out1, start, end, drug_2, id_var = id, drug_2_state)
    expect_equal(tv_out1$drug_1_state,  c(0,0,0,0,1,0,1))
    expect_equal(tv_out2$drug_1_state,  c(rep(0,7), 1, 0, 1))
    expect_equal(tv_out2$drug_2_state,  c(0,0,1,0,1,0,1,1,0,0))
})

test_that("Start-stop tv-covariates cut correctly", {
    tv_out3 <- cut_tv(tv_test, start, end, drug_3_start, id_var = id, drug_3_state)
    tv_out4 <- cut_tv(tv_out3, start, end, drug_3_stop, id_var = id, drug_3_state)
    expect_equal(tv_out3$drug_3_state,  rep(c(0, 1), 5))
    expect_equal(tv_out3$start,  c(0, 110, 0, 110, 0, 111, 0, 109, 0, 110))
    expect_equal(nrow(tv_out4),  15)
    expect_equal(tv_out4$drug_3_state,  rep(c(0, 1, 0), 5))
    expect_equal(tv_out4$start,  c(0, 110, 400, 0, 110, 400, 0, 111, 400, 0, 109, 400, 0, 110, 400))
    expect_equal(tv_out4$end,  c(109, 399, 1000, 109, 399, 689, 110, 399, 
                                 1000, 108, 399, 874, 109, 399, 777))
})

test_that("incremental tv-covariates cut and score correctly", {
    inc_1 <- cut_tv(tv_test, start, end, stage_1, id_var = id, disease_stage, on_existing = "inc")
    inc_2 <- cut_tv(inc_1, start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
    expect_equal(inc_1$disease_stage,  c(0, 1, 0, 0, 0, 0))
    expect_equal(inc_2$disease_stage,  c(0, 1, 2, 0, 0, 0, 0))
})

test_that("Multiple tv-covariates cut and score correctly", {
    # first level of incremental covariate
    tv_all1 <- tv_test %>%
        filter(id == 1) %>%
        cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>% 
        cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
        cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
        cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
        cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc")
    # second level of incremental covariate    
    tv_all <- tv_test %>%
        filter(id == 1) %>%
        cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>% 
        cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
        cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
        cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
        cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc") %>%
        cut_tv(start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")
    
    expect_equal(tv_all$drug_3_state, c(0, 1, 1, 0, 0))
    expect_equal(tv_all$start, c(0, 110, 300, 400, 450))
    expect_equal(tv_all1$disease_stage, c(0, 0, 1, 1))
    expect_equal(tv_all$disease_stage, c(0, 0, 1, 1, 2))
})

tv_all <- tv_test %>%
    cut_tv(start, end, drug_1, id_var = id, drug_1_state) %>% 
    cut_tv(start, end, drug_2, id_var = id, drug_2_state) %>%
    cut_tv(start, end, drug_3_start, id_var = id, drug_3_state) %>%
    cut_tv(start, end, drug_3_stop, id_var = id, drug_3_state) %>%
    cut_tv(start, end, stage_1, id_var = id, disease_stage, on_existing = "inc") %>%
    cut_tv(start, end, stage_2, id_var = id, disease_stage, on_existing = "inc")

