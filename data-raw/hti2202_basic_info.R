#------ Overall

hti2202_basic_info <- impactR::import_xlsx("data-raw/hti2202_basic_info.xlsx")

usethis::use_data(hti2202_basic_info, overwrite = TRUE)

#------ Overall

hti2202_zmpap_basic_info <- impactR::import_xlsx("data-raw/hti2202_zmpap_basic_info.xlsx") |>
  mutate(admin3_name = dplyr::case_when(
    admin3_name == "Cite Soleil" ~ "Cité-Soleil",
    admin3_name == "Petion-Ville" ~ "Pétion-Ville",
    TRUE ~ admin3_name
  ))

usethis::use_data(hti2202_zmpap_basic_info, overwrite = TRUE)
