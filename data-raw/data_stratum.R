## code to prepare `my_dataset` dataset goes here
data_stratum <- readr::read_rds("data-raw/hti2202_analysis_wo_west_stratum.RDS")
usethis::use_data(data_stratum, overwrite = TRUE)

