## code to prepare `my_dataset` dataset goes here
data_main <- readr::read_rds("data-raw/hti2202_analysis_wo_west_all.RDS")
usethis::use_data(data_main, overwrite = TRUE)
