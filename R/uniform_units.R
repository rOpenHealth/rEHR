#' Transforms hba1c values to mmol/mol
#'
#' This function converts hba1c and fructosamine values to comon mmol/mol values
#' conversions: mmol/L, mg/dL, fructosamine
#' Assumes doctor miscoding for values < 20 after previous transformations
#'
#' The input dataframe must include the data2, data3 and enttype columns and the enttype must equal
#' either 275 (HbA1C) or 356 (Fructosamine) 
#' 
#'@export
#'
#'@param df a dataframe of hba1c test scores
#'@return dataframe with all hba1c values recoded to mmol/mol
cprd_uniform_hba1c_values <- function(df){
    assert_that(all(c("data2", "data3", "enttype") %in% names(df)), 
                all(df$enttype %in% c(275, 356)))
    df$hba1c_score <- df$data2
    df <- df[complete.cases(df$hba1c_score),]
    # mmol/L to %
    df$hba1c_score[df$data3 == 96] <- (df$hba1c_score[df$data3 == 96] + 2.59) / 1.59
    # mg/dL to %
    df$hba1c_score[df$data3 == 82] <- (df$hba1c_score[df$data3 == 82] + 46.7) / 28.7
    # % to mmol/mol
    df$hba1c_score[df$data3 %in% c(1, 96, 82)] <- (df$hba1c_score[df$data3 %in% c(1, 96, 82)] - 2.15) * 10.929
    # deal with the data not entered
    df$hba1c_score[df$data3==0 & df$data2 < 20] <- (df$hba1c_score[df$data3==0 & df$data2 < 20] - 2.15) * 10.929
    df$hba1c_score[df$data3==0 & df$data2 > 150] <- NA
    # Deal with fructosamine values:
    df$hba1c_score[df$enttype == 356] <-  ((0.017 * df$hba1c_score[df$enttype == 356] + 1.61) - 2.15) * 10.929
    df$hba1c_score[!df$data3 %in% c(0, 1, 82, 96, 97) & df$enttype != 356 ] <- NA
    df <- df[complete.cases(df$hba1c_score),]
    # deal with doctor miscoding following 
    df$hba1c_score[df$hba1c_score > 0 & df$hba1c_score < 20] <-  
        (df$hba1c_score[df$hba1c_score > 0 & df$hba1c_score < 20] - 2.15) * 10.929
    df <- df[df$hba1c_score > 0 & df$hba1c_score < 150,]
    if(nrow(df)){
        return(df)
    } else return(NULL)
}

