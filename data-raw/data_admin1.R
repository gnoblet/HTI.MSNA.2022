## code to prepare `my_dataset` dataset goes here
data_admin1 <- readr::read_rds("data-raw/hti2202_analysis_wo_zmpap_admin1.RDS")

level_sub_rq <- unique(data_admin1$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_admin1$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_admin1 <- data_admin1 |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_admin1, overwrite = TRUE)
