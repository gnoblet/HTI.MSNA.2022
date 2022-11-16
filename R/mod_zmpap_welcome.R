#' zmpap_welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zmpap_welcome_ui <- function(id){

  ns <- NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      leaflet::leafletOutput(ns("map"), height = "650px"),
      shiny::absolutePanel(
        id = "presentation-zmpap",
        class = "well",
        fixed = TRUE,
        draggable = F,
        top = 120,
        left = 30,
        right = "auto",
        width = 320,
        shiny::h2("Zone métropolitaine de Port-au-Prince"),
        shiny::p("Les différents acteurs de la coordination humanitaire ont exprimé le besoin d'avoir à disposition des données plus granulaires pour la Zone métropolitaine de Port-au-Prince. L'échantillon est stratifié par grappes avec un niveau de confiance de 95% et une marge d'erreur de 10%. Les données sont disponibles au niveau des communes. La collecte de données a eu lieu du 9 août au 13 septembre et 1188 ménages de la zone métropolitaine ont participé à l'enquête."),
        shiny::p("Les communes de Carrefour, Port-au-Prince, Delmas, Petion-Ville, Cite-Soleil et Thomazeau sont incluses dans l'échantillon pour la ZMPAP. Pour les communes de Carrefour et de Thomazeau, seules les zones urbaines rattachées à la zone métropolitaine sont considérés. Pour Thomazeau notamment, 37 entretiens ont été menés."),
        shiny::br(),
        shiny::img(src = "www/reach_logo.png", width = "60%", align = "left")
      )
    )
  )
}

#' zmpap_welcome Server Functions
#'
#' @noRd
mod_zmpap_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")
    dark_grey <- visualizeR::cols_reach("dk_grey")


    #------ Spatial stratum
    stratum_zmpap_spatial <- HTI.MSNA.2022::hti_stratum_zmpap |>
      janitor::clean_names() |>
      dplyr::filter(commune != "Gressier") |>
      mutate(commune = dplyr::case_when(
        commune == "Cité-Soleil" ~ "Cite-Soleil",
        commune == "Pétion-Ville" ~ "Petion-Ville",
        commune == "Croix-Des-Bouquets" ~ "Croix-des-Bouquets",
        TRUE ~ commune
      )) |>
      sf::st_make_valid()

    stratum_centroid <- stratum_zmpap_spatial |>
      sf::st_centroid()

    stratum_line <- stratum_zmpap_spatial |>
      sf::st_cast("MULTILINESTRING")

    stratum_labels_halo <- sprintf(
      '<strong><span class = "leaflet-admin1"; style="font-size: 15px; color: %s">%s</span></strong>',
      main_grey,  stratum_centroid$commune
    ) |>
      lapply(htmltools::HTML)



    #------ Spatial stratum
    stratum <- HTI.MSNA.2022::hti_stratum_out_of_zmpap |>
      janitor::clean_names()

    stratum_zmpap <- HTI.MSNA.2022::hti_stratum_zmpap |>
      janitor::clean_names() |>
      dplyr::filter(commune != "Gressier") |>
      dplyr::mutate(milieu = "ZMPAP")

    #------ Basic info : zmpap

    basic_info_zmpap <- HTI.MSNA.2022::hti2202_zmpap_basic_info

    stratum_zmpap <- dplyr::left_join(
      stratum_zmpap,
      basic_info_zmpap,
      by = c("commune" = "admin3_name")
    ) |>
      dplyr::mutate(stratum = "OUEST - Urbain")

    #------ Map

    output$map <- leaflet::renderLeaflet({


      label_stratum_zmpap <- sprintf(
        "<div class ='leaflet-hover'>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> ZMPAP </span><br>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Nombre d'entretiens : </strong> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Dates de collecte : </strong>  %s - %s </span><br>
        </div>
        ",
        main_red,
        main_grey,
        stratum_zmpap$stratum,
        main_red,
        stratum_zmpap$commune,
        main_grey,
        stratum_zmpap$n,
        main_grey,
        stratum_zmpap$start_name,
        stratum_zmpap$end_name
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
          dragging = TRUE,
          scrollWheelZoom = FALSE,
          easeLinearity = 0.35,
          minZoom = 11,
          maxZoom = 11.3
        )
      ) |>

        #------ Providers
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
                                  options = leaflet::providerTileOptions(opacity = 0.5)
        ) |>


        #------ Polygons - ZMPAP

        leaflet::addPolygons(
          data = stratum_zmpap,
          fillColor = "#877c60",
          color = main_lt_grey,
          weight = 0.5,
          smoothFactor = 0.5,
          opacity = 0.9,
          fillOpacity = 0.9,
          options = list(zIndex = 400),
          label = label_stratum_zmpap,

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

        #------ Limites administratives de strate
        leaflet::addPolylines(
          data = stratum_line,
          color = "#c4c4c4",
          weight = 1.5,
          opacity = 1.0,
          options = list(zIndex = 400)
        ) |>
        leaflet::addLabelOnlyMarkers(
          data = stratum_centroid,
          label = stratum_labels_halo,
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
        leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE))
    })

  })
}

## To be copied in the UI
# mod_zmpap_welcome_ui("zmpap_welcome_1")

## To be copied in the server
# mod_zmpap_welcome_server("zmpap_welcome_1")
