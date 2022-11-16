#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- NS(id)

  shiny::tabPanel("Introduction",
    icon = shiny::icon("flag"),
    value = "panel-welcome",
    div(
      class = "outer",
      leaflet::leafletOutput(ns("map"), height = "100%"),
      shiny::absolutePanel(
        id = "presentation",
        class = "well",
        fixed = TRUE,
        draggable = F,
        top = 90,
        left = 30,
        right = "auto",
        width = 320,
        shiny::h2("Présentation du projet"),
        shiny::p("La nature multiforme de la crise en Haïti explique que la coordination humanitaire estime à 4,9 millions le nombre de personnes dans le besoin en 2022. Tandis que l’accès humanitaire représente un obstacle de plus en plus tangible à la collecte d’information, en raison du caractère enclavé de certaines zones et du contexte sécuritaire volatile, les données disponibles sont généralement spécifiques à une intervention, un lieu ou un secteur."),
        shiny::p("Afin de répondre à ces défis en termes de gestion de l’information, REACH, sous le mandat du GCIS a facilité pour la première fois en Haïti une Evaluation multisectorielle des besoins (MSNA) qui couvre l'ensemble du territoire afin d'informer le Cycle de programmation humanitaire (HPC) 2023."),
        shiny::br(),
        shiny::img(src = "www/reach_logo.png", width = "60%", align = "left")
      ),
      shiny::absolutePanel(
        id = "method",
        class = "well",
        fixed = TRUE,
        draggable = F,
        top = 90,
        left = "auto",
        right = 30,
        width = 320,
        shiny::h2("Méhodologie"),
        shiny::p("La collecte de données a eu lieu du 12 juin au 13 septembre 2022. 3896 ménages ont participé à l'enquête, dont 1188 dans la Zone métropolitaine de Port-au-Prince. Les entretiens ont été effectués en personne."),
        shiny::p("REACH Initiative a effectué la collecte pour les ménages en population générale. L'échantillon est stratifié par grappes avec un niveau de confiance de 95% et une marge d'erreur de 10%. Les données sont disponibles au niveau des départements et des milieux (soit rural, soit urbain), et au niveau des communes pour la Zone métropolitaine de Port-au-Prince. L'échantillon n'a pas pu être complété pour le département de l'Ouest en zone rurale du fait des contraintes sécuritaires de septembre 2022, les résultats sont donc à considérer comme indicatifs. Pour les populations déplacées et rapatriées, les données ont été collectées par l'OIM."),
        shiny::p("Le questionnaire a été mis au point avec les partenaires sectoriels et les groupes de travail thématiques. Il est traduit en créole haïtien. Le questionnaire est disponible ", shiny::tags$a("ici.", href = "https://www.impact-repository.org/document/reach/b2448f66/REACH_HTI_dap_MSNA-2022-1-1.xlsx")),
        shiny::p("Pour plus d'informations, voir l'onglet", shiny::tags$em("A propos."))
      )
    )
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")
    dark_grey <- visualizeR::cols_reach("dk_grey")

    admin0_frontier <- HTI.MSNA.2022::hti_admin0_frontier
    admin0_border <- HTI.MSNA.2022::hti_admin0_border

    #----- Spatial data
    admin1_polygon <-  HTI.MSNA.2022::hti_admin1_polygon

    admin1_line <-  HTI.MSNA.2022::hti_admin1_line

    admin1_centroid <- admin1_line |>
      sf::st_point_on_surface()

    admin1_labels_halo <- sprintf(
      '<strong><span style="font-size: 18px; color: %s">%s</span></strong>',
      main_grey, admin1_centroid$ADM1_FR
    ) |>
      lapply(htmltools::HTML)

    #------ Spatial stratum
    stratum <- HTI.MSNA.2022::hti_stratum_out_of_zmpap |>
      janitor::clean_names()

    stratum_zmpap <- HTI.MSNA.2022::hti_stratum_zmpap |>
      janitor::clean_names() |>
      dplyr::filter(commune != "Gressier") |>
      dplyr::mutate(milieu = "ZMPAP")


    #------ Basic info : overall
    basic_info <- HTI.MSNA.2022::hti2202_basic_info |>
      dplyr::rename(milieu_id = milieu)

    stratum <- dplyr::left_join(
      stratum,
      basic_info,
      by = c("strate" = "stratum")
    )


    #------ Basic info : zmpap

    basic_info_zmpap <- HTI.MSNA.2022::hti2202_zmpap_basic_info

    stratum_zmpap <- dplyr::left_join(
      stratum_zmpap,
      basic_info_zmpap,
      by = c("commune" = "admin3_name")
    ) |>
      dplyr::mutate(stratum = "OUEST - Urbain")


    #------ Colors factors

    fillcol <- leaflet_color_factor(pal = c(main_grey, main_lt_grey), domain = stratum$milieu)
    fillcol_legend <- leaflet_color_factor(pal = c(main_grey, main_lt_grey), domain = stratum$milieu)


    #------ Map

    output$map <- leaflet::renderLeaflet({

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
          dragging = FALSE,
          scrollWheelZoom = FALSE,
          easeLinearity = 0.35,
          minZoom = 7,
          maxZoom = 11
        )
      ) |>
        #------ Set View
        leaflet::setView(lng = - 73, lat = 19, zoom = 8) |>

        #------ Providers
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
                                  options = leaflet::providerTileOptions(opacity = 0.5)
        ) |>
        #------ Polygons - Overall
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
        )

    })

  })
}
## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
