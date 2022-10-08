## code to prepare `my_dataset` dataset goes here
data_main <- readr::read_rds("data-raw/hti2202_analysis_wo_zmpap_all.RDS")

level_sub_rq <- unique(data_main$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_main$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_main <- data_main |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
    dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_main, overwrite = TRUE)
