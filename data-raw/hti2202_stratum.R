
#----- Overall

hti_stratum_overall <- sf::st_read("data-raw/hti2202_stratum_overall.shp") |>
  sf::st_transform("EPSG:4326")

usethis::use_data(hti_stratum_overall, overwrite = TRUE)



#----- Out of ZMPAP

hti_stratum_out_of_zmpap <- sf::st_read("data-raw/hti2202_stratum_out_of_zmpap.shp") |>
  sf::st_transform("EPSG:4326")

usethis::use_data(hti_stratum_out_of_zmpap, overwrite = TRUE)


#----- ZMPAP

hti_stratum_zmpap <- sf::st_read("data-raw/hti2202_stratum_zmpap.shp") |>
  sf::st_transform("EPSG:4326")

usethis::use_data(hti_stratum_zmpap, overwrite = TRUE)
