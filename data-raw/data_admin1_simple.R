## code to prepare `my_dataset` dataset goes here
data_admin1_simple <- readr::read_rds("data-raw/hti2202_monitoring_analysis_admin1_simple.RDS")
usethis::use_data(data_admin1_simple, overwrite = TRUE)
