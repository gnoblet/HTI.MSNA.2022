## code to prepare `my_dataset` dataset goes here
hti_stratum <- sf::st_read("data-raw/hti2202_stratum.shp") |>
  sf::st_transform("EPSG:4326")

usethis::use_data(hti_stratum, overwrite = TRUE)
