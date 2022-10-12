#' zmpap_indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zmpap_indicator_ui <- function(id){
  ns <- NS(id)

    shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      class = "well-tabset",
      shinyWidgets::prettyRadioButtons(
        inputId = ns("disagg"),
        label = "Niveau géographique",
        choices = c("Métropolitain", "Communal"),
        selected = "Métropolitain",
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
      # shiny::hr(),
      shiny::p(shiny::htmlOutput(ns("infobox")))
    ),
    shiny::mainPanel(
      fixed = TRUE,
      draggable = FALSE,
      width = 9,
      shiny::h3(shiny::textOutput(ns("indicator_name"))),
      reactable::reactableOutput(ns("table"), width = "auto", height = "80%")    )
  )
}

#' zmpap_indicator Server Functions
#'
#' @noRd
mod_zmpap_indicator_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")



    analysis <- reactive({
      switch(input$disagg,
             "Métropolitain" = HTI.MSNA.2022::data_zmpap_all |>
                        mutate_if_nulla(choices_label, " ") |>
                        mutate_if_nulla(stat, 0) |>
                        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))),
              "Communal" = HTI.MSNA.2022::data_zmpap_stratum |>
                        mutate_if_nulla(choices_label, " ") |>
                        dplyr::distinct(id_analysis, choices_label, group_disagg_label, .keep_all = T) |>
                        tidyr::pivot_wider(
                          id_cols = c(id_analysis, analysis_name, rq, sub_rq, indicator, choices, recall, subset, choices_label),
                          names_from = group_disagg_label,
                          values_from = stat
                        ) |>
                        dplyr::mutate(
                          dplyr::across(
                            where(is.numeric),
                            \(x) ifelse(is.na(x), 0, x)
                          )
                        ) |>
                        dplyr::mutate(
                          dplyr::across(
                            where(is.numeric),
                            \(x) ifelse(analysis_name == "Proportion", round(x * 100, 0), round(x, 1))
                          )
                        )
               )
    })


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

    sector <- shiny::reactive({input$rq})
    sub_sector <- shiny::reactive({input$sub_rq})
    indicator <- shiny::reactive({input$indicator})

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
        unique(na.omit(analysis_filtered()$recall))
      )

      subset <-
        ifelse(
          is.na(unique(analysis_filtered()$subset)),
          "Aucun",
          unique(na.omit(analysis_filtered()$subset))
        )

      pop_group <- "Population générale"

      reduced_info_box(
        recall = recall,
        subset = subset,
        prefix_recall = "Période de rappel :",
        prefix_subset = "Sous-ensemble :"
      )
    })


    indicator_name <- shiny::reactive({
      input$indicator
    })

    output$indicator_name <- shiny::renderText(indicator_name())


    output$table <- reactable::renderReactable({
      req(input$rq, input$sub_rq, input$indicator)

      analysis_filtered <- analysis() |>
        dplyr::filter(
          rq == input$rq,
          sub_rq == input$sub_rq,
          indicator == input$indicator
        )

      shiny::validate(shiny::need(
        nrow(analysis_filtered) > 0,
        "Le tableau se met à jour."
      ))

      if (input$disagg == "Métropolitain") {
        filtered <- analysis_filtered |>
          dplyr::select("Type d'analyse" = analysis_name, choices_label, "Statistique" = stat)
      } else if (input$disagg == "Communal") {
        filtered <- analysis_filtered |>
          impactR::deselect(id_analysis, rq, sub_rq, choices, recall, subset, indicator, analysis_name)
      }

      rctbl <- filtered |>
        dplyr::rename("Option de réponse" = choices_label) |>
        dplyr::mutate(
          dplyr::across(
            where(is.character),
            \(x) ifelse(x == " ", NA_character_, x)
          )
        ) |>
        janitor::remove_empty(which = "cols") |>
        as.data.frame()

      if (input$disagg == "Communal") {

        rctbl <- rctbl |>
          reactable::reactable(
            class = "reactable",
            rownames = FALSE,
            defaultPageSize = 50,
            bordered = TRUE,
            striped = TRUE,
            highlight = TRUE,
            columns = list(
              "Option de réponse" = reactable::colDef(minWidth = 150, maxWidth = 350, sticky = "left")
            ),
            style = list(fontSize = "10px"),
            defaultColDef = reactable::colDef(
              vAlign = "center",
              maxWidth = 150,
              cell = reactablefmtr::color_tiles(
                analysis_filtered,
                colors = c("white", "#EE5859"),
                box_shadow = TRUE,
                span = TRUE
              ),
              align = "center"
            )
          )

      } else {

        rctbl <- rctbl |>
          reactable::reactable(
            class = "reactable",
            rownames = FALSE,
            defaultPageSize = 50,
            bordered = TRUE,
            striped = TRUE,
            highlight = TRUE,
            columns = list(
              "Option de réponse" = reactable::colDef(minWidth = 150, maxWidth = 350, sticky = "left")
            ),
            style = list(fontSize = "10px"),
            defaultColDef = reactable::colDef(
              vAlign = "center",
              maxWidth = 150,
              cell = reactablefmtr::color_tiles(
                analysis_filtered,
                colors = c("white", "#EE5859"),
                box_shadow = TRUE,
                span = TRUE
              ),
              align = "center"
            )
          )

      }
      #
      #       rctbl <- rctbl |>
      #         reactablefmtr::add_title(indicator_name(), font_weight = "bold")

      return(rctbl)
    })


    # output$table_ui <- shiny::renderUI({
    #   if (nrow(
    #     analysis_filtered <- analysis() |>
    #       dplyr::filter(
    #         rq == input$rq,
    #         sub_rq == input$sub_rq,
    #         indicator == input$indicator
    #       )) == 0) return
    # }) reactable::reactableOutput(ns("table"), width = "auto", height = "auto")


    # shiny::observeEvent(input$download_table, {
    #   shinyscreenshot::screenshot(id = "table",
    #                               filename = paste0("HTI MSNA 2022 - ", input$disagg, " - ", input$indicator))
    # })
  })
}

## To be copied in the UI
# mod_zmpap_indicator_ui("zmpap_indicator_1")

## To be copied in the server
# mod_zmpap_indicator_server("zmpap_indicator_1")
