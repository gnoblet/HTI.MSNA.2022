#' zmpap_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zmpap_map_ui <- function(id){
  ns <- NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      leaflet::leafletOutput(ns("map"), height = "650px")
    ),
    shiny::absolutePanel(
      fixed = TRUE,
      draggable = FALSE,
      top = 160,
      left = 30,
      right = "auto",
      width = 350,
      class = "well",
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
      )
    ),
    shiny::absolutePanel(
      id = "info_box",
      class = "well",
      fixed = TRUE,
      draggable = F,
      top = 160,
      left = "auto",
      right = 30,
      width = 320,
      shiny::p(shiny::htmlOutput(ns("infobox"))),
      shiny::hr(),
      shiny::actionButton(ns("download_map"), icon = shiny::icon("download"), "Télécharger la carte", style = "font-size: 11px")
    )
  )
}



#' zmpap_map Server Functions
#'
#' @noRd
mod_zmpap_map_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns



    # Server : global variables -----------------------------------------------

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")

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


    # Server : data -----------------------------------------------------------

    analysis <- HTI.MSNA.2022::data_zmpap_stratum |>
               mutate_if_nulla(choices_label, " ")

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
      analysis |>
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

    output$map <- leaflet::renderLeaflet({

      analysis_filtered <- analysis|>
        dplyr::filter(
          rq == input$rq,
          sub_rq == input$sub_rq,
          indicator == input$indicator,
          choices_label == input$choice
        )

      missing_stratum_zmpap <- stratum_zmpap_f() |>
        dplyr::filter(!(stratum_zmpap %in% analysis_filtered$group_disagg)) |>
        dplyr::rename(group_disagg = stratum_zmpap, group_disagg_label = stratum_zmpap_name)

      analysis_filtered <- analysis_filtered |>
        dplyr::bind_rows(missing_stratum_zmpap) |>
        mutate_if_nulla(stat, 0) |>
        dplyr::arrange(dplyr::desc(stat))


      analysis_filtered <- stratum_zmpap_spatial |>
        dplyr::left_join(analysis_filtered, by = c("commune" = "group_disagg_label")) |>
        dplyr::mutate(
          stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)),
          analysis_name = ifelse(analysis_name == "Proportion", "Proportion (%)", analysis_name)
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
      label_zmpap <- sprintf(
        "<div class ='leaflet-hover'>
        <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 11px; color: %s;'> %s </span><br>
        <span style = 'font-size: 11px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 15px; color: %s; font-weight: bold;'> %s </span>
        </div>
        ",
        main_grey,
        analysis_filtered$commune,
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


      admin_line <- stratum_line



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
          minZoom = 11,
          maxZoom = 11.3
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
          label = label_zmpap,

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
        )|>
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
              "font-family" = "Leelawadee UI, Leelawadee",
              "text-shadow" = sprintf("-0.8px 0 %s, 0 0.8px %s, 0.8px 0 %s, 0 -0.8px %s, 0 0 7px %s", white, white, white, white, white)
            )
          )
        ) |>
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



        shiny::observeEvent(input$download_map, {
          shinyscreenshot::screenshot(
            id = "map",
            filename = paste0("HTI MSNA 2022 - ", input$indicator, ifelse(input$choice == " ", "", paste0(" - ", input$choice)), ".png")
          )
        })
      })
    }


## To be copied in the UI
# mod_zmpap_map_ui("zmpap_map_1")

## To be copied in the server
# mod_zmpap_map_server("zmpap_map_1")
