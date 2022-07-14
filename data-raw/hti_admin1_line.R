## code to prepare `my_dataset` dataset goes here
hti_admin1_line <- sf::st_read("data-raw/hti_admin1_line.shp")
usethis::use_data(hti_admin1_line, overwrite = TRUE)
