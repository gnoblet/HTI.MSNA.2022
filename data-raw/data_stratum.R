## code to prepare `my_dataset` dataset goes here
data_stratum <- readr::read_rds("data-raw/hti2202_analysis_wo_zmpap_stratum.RDS")

level_sub_rq <- unique(data_stratum$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_stratum$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_stratum <- data_stratum |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
    dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_stratum, overwrite = TRUE)

