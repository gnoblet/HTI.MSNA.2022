## code to prepare `my_dataset` dataset goes here
hti_admin0_frontier <- sf::st_read("data-raw/hti_admin0_frontier.shp")
usethis::use_data(hti_admin0_frontier, overwrite = TRUE)
