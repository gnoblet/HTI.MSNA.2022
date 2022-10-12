
# Overall -----------------------------------------------------------------


#------ Overall : stratum

data_overall_stratum <- readr::read_rds("data-raw/hti2202_analysis_overall_stratum.RDS")

level_sub_rq <- unique(data_overall_stratum$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_overall_stratum$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_overall_stratum <- data_overall_stratum |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
    dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_overall_stratum, overwrite = TRUE)



#------ Overall : all

data_overall_all <- readr::read_rds("data-raw/hti2202_analysis_overall_all.RDS")

level_sub_rq <- unique(data_overall_all$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_overall_all$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_overall_all <- data_overall_all |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_overall_all, overwrite = TRUE)



#------ Overall : admin1

data_overall_admin1 <- readr::read_rds("data-raw/hti2202_analysis_overall_admin1.RDS")

level_sub_rq <- unique(data_overall_admin1$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_overall_admin1$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_overall_admin1 <- data_overall_admin1 |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_overall_admin1, overwrite = TRUE)


#------ Overall : milieu

data_overall_milieu <- readr::read_rds("data-raw/hti2202_analysis_overall_milieu.RDS")

level_sub_rq <- unique(data_overall_milieu$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_overall_milieu$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_overall_milieu <- data_overall_milieu |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_overall_milieu, overwrite = TRUE)




# ZMPAP -----------------------------------------------------------------



#------ ZMPAP : stratum

data_zmpap_stratum <- readr::read_rds("data-raw/hti2202_analysis_zmpap_stratum.RDS")

level_sub_rq <- unique(data_zmpap_stratum$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_zmpap_stratum$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_zmpap_stratum <- data_zmpap_stratum |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_zmpap_stratum, overwrite = TRUE)



#------ Overall : all

data_zmpap_all <- readr::read_rds("data-raw/hti2202_analysis_zmpap_all.RDS")

level_sub_rq <- unique(data_zmpap_all$sub_rq) |>
  impactR::subvec_not_in("Besoins prioritaires")
level_sub_rq <- c("Besoins prioritaires", level_sub_rq)

level_indicator <- unique(data_zmpap_all$indicator) |>
  impactR::subvec_not_in("% de ménages par type de besoin prioritaire rapporté")
level_indicator <- c("% de ménages par type de besoin prioritaire rapporté", level_indicator)

data_zmpap_all <- data_zmpap_all |>
  dplyr::mutate(
    sub_rq =  factor(
      sub_rq,
      levels = level_sub_rq),
    indicator = factor(
      indicator,
      levels = level_indicator) ) |>
  dplyr::arrange(sub_rq, indicator)

usethis::use_data(data_zmpap_all, overwrite = TRUE)

