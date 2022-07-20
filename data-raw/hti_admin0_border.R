## code to prepare `my_dataset` dataset goes here
hti_admin0_border <- sf::st_read("data-raw/hti_admin0_border.shp") |>
  sf::st_cast("MULTILINESTRING") |>
  sf::st_transform("EPSG:4326")

usethis::use_data(hti_admin0_border, overwrite = TRUE)
