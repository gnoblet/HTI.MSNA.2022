#' map_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_main_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    "Carte",
    value = "panel_map",
    icon = shiny::icon("map-marker"),

    div(class="outer",

      leaflet::leafletOutput(ns("map"), height = "100%"),

      shiny::absolutePanel(
        fixed = TRUE,
        draggable = FALSE,
        top = 90,
        left = 30,
        right = "auto",
        width = 350,
        class = "well",
        shiny::selectInput(
          inputId = ns("rq"),
          label = "Secteur",
          choices = c("Information générale", "Démographie du ménage", "Déplacement", "Washington Group", "Santé", "Education", "Sécurité alimentaire", "Moyens de subsistance", "ABNA", "EPHA", "Protection", "Redevabilité"),
          selected = "EPHA"),
        shiny::selectInput(
          inputId = ns("sub_rq"),
          label = "Sous-secteur",
          choices = "Accès à l'eau",
          selected = "Accès à l'eau"),
        shiny::selectInput(
          inputId = ns("indicator"),
          label = "Indicateur",
          choices = "% de ménages par source d'eau de boisson",
          selected = "% de ménages par source d'eau de boisson"),
        shiny::selectInput(
            inputId = ns("choice"),
            label = "Choix de réponse",
            choices = "Source protégée",
            selected = "Source protégée"),
        shiny::img(src = "www/reach_logo.png", width = "80%", align = "center")
      ) ,

      shiny::absolutePanel(
        id = "info_box",
        class = "well",
        fixed = TRUE,
        draggable = F,
        top = 75,
        left = "auto",
        right = 20,
        width = 320,
        shiny::p(shiny::htmlOutput(ns("infobox")))),
    )

  )
}

#' map_main Server Functions
#'
#' @noRd
mod_map_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



# Server : global variables -----------------------------------------------

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")

    #----- Spatial data
    admin1_polygon <- HTI.MSNA.2022::hti_admin1_polygon |>
      dplyr::mutate(admin1 = stringr::str_replace_all(stringr::str_to_lower(departemen), " |-|'", "_")) |>
      dplyr::mutate(admin1 = ifelse(admin1 == "grande_anse", "grand_anse", admin1))

    admin1_line <- HTI.MSNA.2022::hti_admin1_line

    admin1_centroid <- admin1_line |>
      sf::st_point_on_surface()

    admin1_labels_halo <- sprintf('<strong><span style="font-size: 18px; color: %s">%s</span></strong>',
      main_grey, admin1_centroid$ADM1_FR
    ) |>
      lapply(htmltools::HTML)

    #------ Other data
    analysis <- HTI.MSNA.2022::data_admin1_simple

    analysis <- admin1_polygon |>
      dplyr::left_join(analysis, by = c("admin1" = "group_disagg")) |>
      dplyr::mutate(
        stat = ifelse(is.na(stat), 0, stat),
        choices_label = ifelse(is.na(choices_label), " ", choices_label)
      ) |>
      dplyr::mutate(
        stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)),
        analysis_name = ifelse(analysis_name == "Proportion", "Proportion (%)", analysis_name)
        )


# Server : Observe --------------------------------------------------------


    shiny::observeEvent(input$rq, {
      shiny::updateSelectInput(session,
                               "sub_rq",
                               choices = analysis |>
                                 dplyr::filter(
                                   rq == input$rq
                                 ) |>
                                 dplyr::pull(sub_rq) |>
                                 unique()
      )
    })

    shiny::observeEvent(input$sub_rq, {
      shiny::updateSelectInput(session,
                               "indicator",
                               choices = analysis |>
                                 dplyr::filter(
                                   rq == input$rq,
                                   sub_rq == input$sub_rq
                                   ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })

    shiny::observeEvent(input$indicator, {
      shiny::updateSelectInput(session,
                               "choice",
                               choices = analysis |>
                                 dplyr::filter(
                                   rq == input$rq,
                                   sub_rq == input$sub_rq,
                                   indicator == input$indicator
                                   ) |>
                                 dplyr::pull(choices_label) |>
                                 unique()
        )
    })




# Server : Infobox --------------------------------------------------------

    output$infobox <- shiny::renderUI({

      sector <- input$rq
      sub_sector <- input$sub_rq
      indicator <- input$indicator
      choice <- ifelse(is.null(input$choice) | is.na(input$choice), " ", input$choice)

      analysis_filtered <- analysis |>
        dplyr::filter(sector == rq, sub_sector == sub_rq, choices_label == choice)

      recall <- ifelse(
        is.na(unique(analysis_filtered$recall)),
        "",
        unique(analysis_filtered$recall)
      )

      subset <- ifelse(
        is.na(unique(analysis_filtered$subset)),
        "Calculé sur l'ensemble des ménages",
        unique(analysis_filtered$subset)
        )

      popgroup <- "Population générale"
      shiny::HTML(sprintf("
                          <span style='color: %s; font-size: 26px; line-height: 1.2;'><strong> %s </strong></span>
                          <br>
                          <span style='color: %s; font-size: 22px; line-height: 1.2;'><strong> %s </strong></span>
                          <br>
                          <span style=' font-size: 18px; color: %s'><strong> %s </span>
                          <hr>
                          %s </strong>
                          <br>
                          %s <hr>
                          <span style=' font-size: 16px; color: %s'> <strong> %s </strong> %s </span>
                          <br><strong> %s </strong> %s
                          ",
                   main_red,
                   sector,
                   main_red,
                   sub_sector,
                   white,
                   popgroup,
                   indicator,
                   choice,
                   white,
                   ifelse(subset == "Calculé sur l'ensemble des ménages", "", "Sous-ensemble : "),
                   subset,
                   ifelse(recall == "", "", "Période de rappel : "),
                   recall,
                  '<style type="text/css"> .shiny-html-output { font-size: 16px; color:#FFFFFF
               font-family: Leelawadee UI} </style>'))
    })



# Server : Map ------------------------------------------------------------


    output$map <- leaflet::renderLeaflet({

      analysis_filtered <- analysis |>
        dplyr::filter(rq == input$rq,
                      sub_rq == input$sub_rq,
                      indicator == input$indicator,
                      choices_label == input$choice
        )

      #----- Color

      fillcol <- leaflet_color_bin(red_pal, bins = 3, domain = analysis_filtered$stat)
      fillcol_legend <- leaflet_color_bin(red_pal, bins = 3, domain = analysis_filtered$stat)

      sector <- input$rq
      sub_sector <- input$sub_rq
      indicator <- input$indicator
      choice <- ifelse(is.null(input$choice) | is.na(input$choice), "", input$choice)

      #------ Highlight label
      label_admin1 <- sprintf(
        '<div class="leaflet-hover">
        <strong><span style="font-size: 18px; color: %s;"> %s </span><br>
        <span style="font-size: 14px; color: %s;"> %s </span><br></strong>
        <span style="font-size: 14px; color: %s;"> %s </span><br>
        <strong><span style="font-size: 14px; color: %s;"> %s </span><br>
        <span style="font-size: %s; color: %s;"> %s </span></strong>
        </div>
        ',
        main_grey,
        analysis_filtered$departemen,
        main_grey,
        paste0("Indicateur : ", indicator),
        main_grey,
        ifelse(choice == "", choice, paste0("Choix : ", choice)),
        main_lt_grey,
        ifelse(is.na(analysis_filtered$subset),
                        "Calculé sur l'ensemble des ménages",
                        paste0("Sous-ensemble : ", analysis_filtered$subset)
                      ),
        "18px",
        main_red,
        ifelse(analysis_filtered$analysis_name == "Proportion (%)",
               paste0(analysis_filtered$stat, "%"),
               analysis_filtered$stat)
        ) |>
          lapply(htmltools::HTML)




      leaflet::leaflet(
        analysis_filtered,

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
          minZoom = 8,
          maxZoom = 9)) |>

        #------ Providers
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
                                  options = leaflet::providerTileOptions(opacity = 0.5)) |>

        #------ Polygons
        leaflet::addPolygons(
          color = white,
          fillColor = ~fillcol(stat),
          weight       = 0.2,
          smoothFactor = 0.5,
          opacity      = 0.9,
          fillOpacity  = 0.9,
          options      = list(zIndex = 400),
          label = label_admin1,

          #------ Highlight
          highlightOptions = leaflet::highlightOptions(fillColor = "main_lt_grey",
                                              color        = "main_lt_grey",
                                              weight       = 2,
                                              opacity      = 0.9,
                                              fillOpacity  = 0.5,
                                              bringToFront = F),
          labelOptions = leaflet::labelOptions(
            noHide = FALSE,
            noWrap = FALSE,
            opacity = 0.9,
            direction = "auto",
            offset = c(-10,0),
            textOnly = F,
            style = list(
              "padding" = "3px 8px",
              "font-family" = "Leelawadee UI",
              "border-color" = "#EE5859"
            )
          )
        ) |>

        #------ Limites administratives
        leaflet::addPolylines(
          data = admin1_line,
          color = main_lt_grey,
          weight = 1.3,
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

        #------ Legend
        leaflet::addLegend(
          position = "bottomright",
          opacity = 1,
          pal = fillcol_legend,
          values = ~stat,
          na.label = "Données manquantes",
          labFormat = label_format(),
          title = unique(na.omit(analysis_filtered$analysis_name))
        ) |>
        leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE))
      })

  })
}

## To be copied in the UI
# mod_map_main_ui("map_main_1")

## To be copied in the server
# mod_map_main_server("map_main_1")
