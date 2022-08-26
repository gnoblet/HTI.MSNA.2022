## code to prepare `my_dataset` dataset goes here
data_milieu <- readr::read_rds("data-raw/hti2202_analysis_wo_west_milieu.RDS")
usethis::use_data(data_milieu, overwrite = TRUE)
