## code to prepare `my_dataset` dataset goes here
data_main_simple <- readr::read_rds("data-raw/hti2202_monitoring_analysis_main_simple.RDS")
usethis::use_data(data_main_simple, overwrite = TRUE)
