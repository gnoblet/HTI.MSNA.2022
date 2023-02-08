#' zmpap_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zmpap_graph_ui <- function(id){
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
      shiny::conditionalPanel(
        condition = "input.disagg == 'Communal'",
        ns = ns,
        shiny::selectInput(
          inputId = ns("choice"),
          label = "Option de réponse",
          choices = "Abris / logement / habitat"
        )
      ),
      # shiny::hr(),
      shiny::p(shiny::htmlOutput(ns("infobox")))
    ),
    shiny::mainPanel(
      fixed = TRUE,
      draggable = FALSE,
      width = 9,
      shiny::br(),
      # shiny::h3(shiny::textOutput(ns("indicator_name"))),
      shiny::plotOutput(ns("graph"), width = "80%", height = "600px") #
    )
)
}

#' zmpap_graph Server Functions
#'
#' @noRd
mod_zmpap_graph_server <- function(id){

  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")

    #------ Plotly things
    # download_button <- list(
    #   name = "Télécharger le graphique en svg",
    #   icon = shiny::icon("download"),
    #   click = htmlwidgets::JS(
    #     "function(gd) {window.open('http://www.mastercard.com', '_blank')}"
    #   )
    # )

    analysis <- reactive({
      switch(input$disagg,
             "Métropolitain" = HTI.MSNA.2022::data_zmpap_all |>
               mutate_if_nulla(choices_label, " ") |>
               mutate_if_nulla(stat, 0) |>
               dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
               dplyr::mutate(choices_label = dplyr::case_when(
                  analysis_name == "Moyenne" ~ "Moyenne",
                  analysis_name == "Médiane" ~ "Médiane",
                  analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                TRUE ~ choices_label)),
            "Communal" = HTI.MSNA.2022::data_zmpap_stratum |>
               mutate_if_nulla(choices_label, " ") |>
               dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
               dplyr::mutate(choices_label = dplyr::case_when(
                 analysis_name == "Moyenne" ~ "Moyenne",
                 analysis_name == "Médiane" ~ "Médiane",
                 analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                 TRUE ~ choices_label))
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

    shiny::observeEvent(input$indicator, {
      shiny::updateSelectInput(session,
                               "choice",
                               choices = analysis() |>
                                 dplyr::filter(
                                   rq == input$rq,
                                   sub_rq == input$sub_rq,
                                   indicator == input$indicator
                                 ) |>
                                 dplyr::pull(choices_label) |>
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
        length(unique(na.omit(analysis_filtered()$recall))) == 0,
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
        # pop_group = pop_group,
        recall = recall,
        subset = subset,
        # prefix_pop_group = "Groupe de population : ",
        prefix_recall = "Période de rappel :",
        prefix_subset = "Sous-ensemble :"
      )
    })


    indicator_name <- shiny::reactive({
      input$indicator
    })


    choice_name <- shiny::reactive({
      input$choice
    })


    output$indicator_name <- shiny::renderText(indicator_name())

    output$graph <-
      # plotly::renderPlotly(
      shiny::renderPlot(
        expr = {
          if (input$disagg == "Métropolitain") {

            analysis_filtered <- analysis() |>
              dplyr::filter(
                rq == input$rq,
                sub_rq == input$sub_rq,
                indicator == input$indicator
              ) |>
              mutate_if_nulla(stat, 0) |>
              dplyr::arrange(dplyr::desc(stat))

          } else if (input$disagg == "Communal") {

            analysis_filtered <- analysis() |>
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

          }


          if (nrow(analysis_filtered) == 0) {

            graph <- visualizeR::bar(
              df = tibble::tibble(stat = 100, choices_label = "Données manquantes"),
              x = choices_label,
              y = stat,
              theme = visualizeR::theme_reach(
                font = "Leelawadee",
                text_size = 12,
                title_size = 13,
                panel_background_color = "#FFFFFF",
                grid_y = TRUE,
                text_font_face = "plain"
              )
            )

          } else {
            if (input$disagg == "Métropolitain") {

              graph <- visualizeR::bar(
                df = analysis_filtered,
                x = forcats::fct_reorder(choices_label, stat),
                y = stat,
                add_text = TRUE,
                percent = FALSE,
                theme = visualizeR::theme_reach(
                  font = "Leelawadee",
                  text_size = 12,
                  title_size = 13,
                  panel_background_color = "#FFFFFF",
                  grid_x = TRUE,
                  palette = "primary",
                  text_font_face = "plain")
              ) +
                ggplot2::scale_x_discrete(labels = scales::label_wrap(70))

            } else if (input$disagg == "Communal") {

              graph <- visualizeR::bar(
                df = analysis_filtered,
                x = forcats::fct_reorder(group_disagg_label, stat),
                y = stat,
                add_text = TRUE,
                percent = FALSE,
                theme = visualizeR::theme_reach(
                  font = "Leelawadee",
                  text_size = 12,
                  title_size = 13,
                  panel_background_color = "#FFFFFF",
                  grid_x = TRUE,
                  palette = "primary",
                  text_font_face = "plain")
              ) +
                ggplot2::scale_x_discrete(labels = scales::label_wrap(70))

            }
          }

          if (input$disagg == "Communal")  graph <- graph + ggplot2::ggtitle(indicator_name(), subtitle = paste("Option de réponse :",  choice_name(), sep = " "))

          if (input$disagg == "Métropolitain")  graph <- graph + ggplot2::ggtitle(indicator_name())

          return(graph)
        }
      )

    shiny::observeEvent(input$download_graph, {
      shinyscreenshot::screenshot(
        id = "graph",
        filename = paste0("HTI MSNA 2022 - ZMPAP - ", input$disagg, " - ", input$indicator, ifelse(input$disagg != "Métropolitain", paste0(" - ", input$choice), ""))
      )
    })
  })

}

## To be copied in the UI
# mod_zmpap_graph_ui("zmpap_graph_1")

## To be copied in the server
# mod_zmpap_graph_server("zmpap_graph_1")
