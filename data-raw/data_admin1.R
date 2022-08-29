## code to prepare `my_dataset` dataset goes here
data_admin1 <- readr::read_rds("data-raw/hti2202_analysis_wo_west_admin1.RDS")
usethis::use_data(data_admin1, overwrite = TRUE)
