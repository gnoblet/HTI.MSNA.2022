## code to prepare `my_dataset` dataset goes here
hti_admin1_polygon <- sf::st_read("data-raw/hti_admin1_polygon.shp")
usethis::use_data(hti_admin1_polygon, overwrite = TRUE)
