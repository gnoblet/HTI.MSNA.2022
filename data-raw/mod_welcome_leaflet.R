library(impactR)
library(visualizeR)
library(sf)
library(dplyr)
library(leaflet)


#------ Colors
white <- visualizeR::cols_reach("white")
red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
main_red <- visualizeR::cols_reach("main_red")
main_grey <- visualizeR::cols_reach("main_grey")
main_lt_grey <- visualizeR::cols_reach("main_lt_grey")
dark_grey <- visualizeR::cols_reach("dk_grey")

admin0_frontier <- sf::st_read("data-raw/hti_admin0_frontier.shp")
admin0_border <- sf::st_read("data-raw/hti_admin0_border.shp") |>
  sf::st_cast("MULTILINESTRING") |>
  sf::st_transform("EPSG:4326")

#----- Spatial data
admin1_polygon <-  sf::st_read("data-raw/hti_admin1_polygon.shp") |>
  dplyr::mutate(admin1 = stringr::str_replace_all(stringr::str_to_lower(departemen), " |-|'", "_")) |>
  dplyr::mutate(admin1 = ifelse(admin1 == "grande_anse", "grand_anse", admin1))

admin1_line <-  sf::st_read("data-raw/hti_admin1_line.shp")

admin1_centroid <- admin1_line |>
  sf::st_point_on_surface()

admin1_labels_halo <- sprintf(
  '<strong><span style="font-size: 18px; color: %s">%s</span></strong>',
  main_grey, admin1_centroid$ADM1_FR
) |>
  lapply(htmltools::HTML)

#------ Spatial stratum
stratum <- sf::st_read("data-raw/hti2202_stratum.shp") |>
  sf::st_transform("EPSG:4326") |>
  janitor::clean_names()

#------ Basic info
basic_info <- impactR::import_xlsx("data-raw/hti2202_basic_info.xlsx") |>
  dplyr::rename(milieu_id = milieu)

stratum <- dplyr::left_join(
  stratum,
  basic_info,
  by = c("strate" = "stratum")
)

source("R/fct_helpers.R")

fillcol <- leaflet_color_factor(pal = c(main_grey, main_lt_grey), domain = stratum$milieu)
fillcol_legend <- leaflet_color_factor(pal = c(main_grey, main_lt_grey), domain = stratum$milieu)


#------ Map

welcome_map <- {
  label_stratum <- sprintf(
    "<div class ='leaflet-hover'>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Nombre d'entretiens : </strong> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Dates de collecte : </strong>  %s - %s </span><br>
        </div>
        ",
    main_grey,
    stratum$department,
    main_red,
    stratum$milieu,
    main_grey,
    stratum$n,
    main_grey,
    stratum$start_name,
    stratum$end_name
  ) |>
    lapply(htmltools::HTML)


  leaflet::leaflet(
    stratum,

    #------ Options
    options = leaflet::leafletOptions(
      zoomControl = FALSE,
      doubleClickZoom = FALSE,
      zoomSnap = 0.01,
      zoomDelta = 0.01,
      attributionControl = FALSE,
      dragging = FALSE,
      scrollWheelZoom = FALSE,
      easeLinearity = 0.35,
      minZoom = 7,
      maxZoom = 8
    )
  ) |>
    #------ Set View

    #------ Providers
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
                              options = leaflet::providerTileOptions(opacity = 0.5)
    ) |>
    #------ Polygons
    leaflet::addPolygons(
      fillColor = ~ fillcol(milieu),
      color = main_lt_grey,
      weight = 0.2,
      smoothFactor = 0.5,
      opacity = 0.9,
      fillOpacity = 0.9,
      options = list(zIndex = 400),
      label = label_stratum,

      #------ Highlight
      highlightOptions = leaflet::highlightOptions(
        fillColor = main_red,
        color = main_red,
        weight = 2,
        opacity = 0.9,
        fillOpacity = 0.5,
        bringToFront = F
      ),
      labelOptions = leaflet::labelOptions(
        noHide = FALSE,
        noWrap = FALSE,
        opacity = 0.9,
        direction = "auto",
        offset = c(-10, 0),
        textOnly = F,
        style = list(
          "padding" = "3px 8px",
          "font-family" = "Leelawadee UI, Leelawadee",
          "border-color" = "#EE5859"
        )
      )
    ) |>
    #------ Limites administratives
    leaflet::addPolylines(
      data = admin1_line,
      color = dark_grey,
      weight = 1.3,
      opacity = 1.0,
      options = list(zIndex = 400)
    ) |>
    #------- Limites administratives : contour
    leaflet::addPolylines(
      data = admin0_border,
      color = "#000000",
      fillOpacity = 0,
      weight = 1.5,
      opacity = 1.0,
      options = list(zIndex = 400)
    ) |>
    #------- Limites administratives : frontière
    leaflet::addPolylines(
      data = admin0_frontier,
      color = "#000000",
      weight = 2,
      opacity = 1.0,
      options = list(zIndex = 400)
    ) |>
    #------ Label Admin 1 :
    leaflet::addLabelOnlyMarkers(
      data = admin1_centroid,
      label = admin1_labels_halo,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list(
          "padding"     = "3px 8px",
          "font-family" = "Leelawadee UI",
          "text-shadow" = sprintf("-0.8px 0 %s, 0 0.8px %s, 0.8px 0 %s, 0 -0.8px %s, 0 0 7px %s", white, white, white, white, white)
        )
      )
    ) |>
    leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE)) |>
    #------ Legend
    leaflet::addLegend(
      position = "bottomright",
      opacity = 1,
      pal = fillcol_legend,
      values = ~milieu,
      na.label = "Données manquantes",
      title = "Milieu"
    ) |>
    leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE))
}

usethis::use_data(welcome_map, overwrite = TRUE)

