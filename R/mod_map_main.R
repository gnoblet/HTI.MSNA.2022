#' map_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_main_ui <- function(id) {
  ns <- NS(id)

  shiny::tabPanel(
    "Carte",
    value = "panel_map",
    icon = shiny::icon("location-dot"),
    div(
      class = "outer",
      leaflet::leafletOutput(ns("map"), height = "100%"),
      shiny::absolutePanel(
        fixed = TRUE,
        draggable = FALSE,
        top = 90,
        left = 30,
        right = "auto",
        width = 350,
        class = "well",
        shinyWidgets::prettyRadioButtons(
          inputId = ns("disagg"),
          label = "Désagrégation par milieu",
          choices = c("Ensemble", "Rural et urbain"),
          selected = "Ensemble",
          fill = TRUE,
          status = "danger"
        ),
        shiny::selectInput(
          inputId = ns("rq"),
          label = "Secteur",
          choices = c("Information générale", "Démographie du ménage", "Déplacement", "Washington Group", "Santé", "Education", "Sécurité alimentaire", "Moyens de subsistance", "ABNA", "EPHA", "Protection", "Redevabilité"),
          selected = "Redevabilité"
        ),
        shiny::selectInput(
          inputId = ns("sub_rq"),
          label = "Sous-secteur",
          choices = "Besoins prioritaires"
        ),
        shiny::selectInput(
          inputId = ns("indicator"),
          label = "Indicateur",
          choices = "% de ménages par type de besoin prioritaire rapporté"
        ),
        shiny::selectInput(
          inputId = ns("choice"),
          label = "Option de réponse",
          choices = "Abris / logement / habitat"
        ),
        shiny::img(src = "www/reach_logo.png", width = "60%", align = "left")
      ),
      shiny::absolutePanel(
        id = "info_box",
        class = "well",
        fixed = TRUE,
        draggable = F,
        top = 90,
        left = "auto",
        right = 30,
        width = 320,
        shiny::p(shiny::htmlOutput(ns("infobox"))),
        shiny::hr(),
        shiny::actionButton(ns("download_map"), icon = shiny::icon("download"), "Télécharger la carte", style = "font-size: 11px")
      )
    )
  )
}

#' map_main Server Functions
#'
#' @noRd
mod_map_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
      dplyr::mutate(admin1 = stringr::str_replace_all(stringr::str_to_lower(departemen), " |-|'", "_"))

    admin1_line <- HTI.MSNA.2022::hti_admin1_line

    admin1_centroid <- admin1_line |>
      sf::st_point_on_surface()

    admin1_labels_halo <- sprintf(
      '<strong><span class = "leaflet-admin1"; style="font-size: 15px; color: %s">%s</span></strong>',
      main_grey, admin1_centroid$ADM1_FR
    ) |>
      lapply(htmltools::HTML)

    admin0_frontier <- HTI.MSNA.2022::hti_admin0_frontier

    admin0_border <- HTI.MSNA.2022::hti_admin0_border


    #------ Spatial stratum
    stratum <- HTI.MSNA.2022::hti_stratum_overall |>
      janitor::clean_names()

    stratum_line <- stratum |>
      sf::st_cast("MULTILINESTRING")



    # Server : data -----------------------------------------------------------

    analysis <- reactive({
      switch(input$disagg,
        "Ensemble" = HTI.MSNA.2022::data_overall_admin1 |>
          mutate_if_nulla(choices_label, " "),
        "Rural et urbain" = HTI.MSNA.2022::data_overall_stratum |>
          mutate_if_nulla(choices_label, " ")
      )
    })

    # Server : Observe --------------------------------------------------------


    shiny::observeEvent(input$rq, {
      shiny::updateSelectInput(session,
        "sub_rq",
        choices = analysis() |>
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
        choices = analysis() |>
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
        choices = analysis() |>
          dplyr::filter(
            rq == input$rq,
            sub_rq == input$sub_rq,
            indicator == input$indicator
          ) |>
          dplyr::arrange(choices_label) |>
          dplyr::pull(choices_label) |>
          unique()
      )
    })


    # Server : Infobox --------------------------------------------------------

    sector <- shiny::reactive({input$rq})
    sub_sector <- shiny::reactive({input$sub_rq})
    indicator <- shiny::reactive({input$indicator})
    choice <- shiny::reactive({input$choice})

    analysis_filtered <- shiny::reactive({
      analysis() |>
        dplyr::filter(
          sector() == rq,
          sub_sector() == sub_rq,
          indicator() == indicator)
    })

    output$infobox <- shiny::renderUI({


      recall <- ifelse(
        is.na(unique(analysis_filtered()$recall)),
        "Aucune",
        unique(analysis_filtered()$recall)
      )

      subset <-
        ifelse(
          is.na(unique(analysis_filtered()$subset)),
          "Aucun",
          unique(analysis_filtered()$subset)
        )

      pop_group <- "Population générale"

      info_box(
        main_title = sector(),
        sub_title = sub_sector(),
        # pop_group = pop_group,
        indicator = indicator(),
        choice = choice(),
        recall = recall,
        subset = subset,
        prefix_recall = "Période de rappel :",
        prefix_subset = "Sous-ensemble :"
      )
    })



    # Server : Map ------------------------------------------------------------

    choice_map <- shiny::reactive({
      analysis_filtered <- analysis() |>
        dplyr::filter(
          rq == input$rq,
          sub_rq == input$sub_rq,
          indicator == input$indicator,
          choices_label == input$choice
        )

      missing_admin <- switch(input$disagg,
        "Ensemble" = admin1_f() |>
          dplyr::filter(!(admin1 %in% analysis_filtered$group_disagg)) |>
          dplyr::rename(group_disagg = admin1, group_disagg_label = admin1_name),
        "Rural et urbain" = stratum_f() |>
          dplyr::filter(!(stratum %in% analysis_filtered$group_disagg)) |>
          dplyr::rename(group_disagg = stratum, group_disagg_label = stratum_name)
      )

      analysis_filtered <- analysis_filtered |>
        dplyr::bind_rows(missing_admin) |>
        mutate_if_nulla(stat, 0) |>
        dplyr::arrange(dplyr::desc(stat))


      analysis_filtered <- switch(input$disagg,
        "Ensemble" = admin1_polygon |>
          dplyr::left_join(analysis_filtered, by = c("admin1" = "group_disagg")) |>
          dplyr::mutate(
            stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)),
            analysis_name = ifelse(analysis_name == "Proportion", "Proportion (%)", analysis_name)
          ) ,
        "Rural et urbain" = stratum |>
          dplyr::left_join(
            analysis_filtered |> dplyr::mutate(
              milieu = ifelse(stringr::str_detect(group_disagg, "_urbain"), "Urbain", "Rural"),
              admin1 = stringr::str_remove_all(group_disagg_label, " -.*")
            ),
            by = c("strate" = "group_disagg", "milieu")
          ) |>
          dplyr::mutate(
            stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)),
            analysis_name = ifelse(analysis_name == "Proportion", "Proportion (%)", analysis_name)
          )
      ) |>
        mutate_if_nulla(stat, 0)


      #----- Color

      fillcol <- leaflet_color_bin(red_pal, bins = 3, domain = analysis_filtered$stat)
      fillcol_legend <- leaflet_color_bin(red_pal, bins = 3, domain = analysis_filtered$stat)

      sector <- input$rq
      sub_sector <- input$sub_rq
      indicator <- input$indicator
      choice <- input$choice

      #------ Highlight label
      label <- switch(input$disagg,
        "Ensemble" = sprintf(
          "<div class ='leaflet-hover'>
            <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span>
            </div>
            ",
          main_grey,
          analysis_filtered$departemen,
          main_grey,
          indicator,
          main_grey,
          ifelse(choice == " ", "", choice),
          main_lt_grey,
          ifelse(is.na(analysis_filtered$subset),
            "Calculé sur l'ensemble des ménages",
            paste0("Sous-ensemble : ", analysis_filtered$subset)
          ),
          main_red,
          ifelse(is.na(analysis_filtered$analysis_name),
            "0%",
            ifelse(analysis_filtered$analysis_name == "Proportion (%)",
              paste0(analysis_filtered$stat, "%"),
              analysis_filtered$stat
            )
          )
        ) |>
          lapply(htmltools::HTML),
        "Rural et urbain" = sprintf(
          "<div class ='leaflet-hover'>
            <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s;'> %s </span><br>
            <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
            <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span>
            </div>
            ",
          main_grey,
          analysis_filtered$admin1,
          main_grey,
          analysis_filtered$milieu,
          main_grey,
          indicator,
          main_grey,
          ifelse(choice == " ", "", choice),
          main_lt_grey,
          ifelse(is.na(analysis_filtered$subset),
            "Calculé sur l'ensemble des ménages",
            paste0("Sous-ensemble : ", analysis_filtered$subset)
          ),
          main_red,
          ifelse(is.na(analysis_filtered$analysis_name),
            "0%",
            ifelse(analysis_filtered$analysis_name == "Proportion (%)",
              paste0(analysis_filtered$stat, "%"),
              analysis_filtered$stat
            )
          )
        ) |>
          lapply(htmltools::HTML)
      )

      admin_line <- switch(input$disagg,
        "Ensemble" = admin1_line,
        "Rural et urbain" = stratum_line
      )


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
          minZoom = 7,
          maxZoom = 8
        )
      ) |>
        #------ Providers
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
          options = leaflet::providerTileOptions(opacity = 0.5)
        ) |>
        #------ Polygons
        leaflet::addPolygons(
          opacity = 0,
          fillColor = ~ fillcol(stat),
          color = "#c4c4c4",
          weight = 2,
          smoothFactor = 0.5,
          # opacity      = 0.9,
          fillOpacity = 0.9,
          options = list(zIndex = 400),
          label = label,

          #------ Highlight
          highlightOptions = leaflet::highlightOptions(
            fillColor = main_lt_grey,
            color = "#c4c4c4",
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
          data = admin_line,
          color = "#c4c4c4",
          weight = 1.5,
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
              "font-family" = "Leelawadee UI, Leelawadee",
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

    output$map <- leaflet::renderLeaflet({
      choice_map()
    })




    shiny::observeEvent(input$download_map, {
      shinyscreenshot::screenshot(
        id = "map",
        filename = paste0("HTI MSNA 2022 - ", input$indicator, ifelse(input$choice == " ", "", paste0(" - ", input$choice)), ".png")
      )
    })
  })
}

## To be copied in the UI
# mod_map_main_ui("map_main_1")

## To be copied in the server
# mod_map_main_server("map_main_1")
