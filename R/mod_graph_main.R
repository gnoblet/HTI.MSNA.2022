#' graph_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_main_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    "Graphes",
    value = "panel-graph",
    icon = shiny::icon("chart-bar"),

    div(class="outer",

        shiny::absolutePanel(
          fixed = TRUE,
          draggable = FALSE,
          top = 90,
          left = 400,
          right = "auto",
          width = 1000,
          plotly::plotlyOutput(ns("graph"), height = "700px")
        ),

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
            label = "Niveau géographique",
            choices = c("National", "Départemental"),
            selected = "National",
            fill = TRUE,
            status = "danger"),
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
          shiny::conditionalPanel(
            condition = "input.disagg == 'Départemental'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("choice"),
              label = "Choix de réponse",
              choices = "Source protégée",
              selected = "Source protégée")
            )
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
          actionButton(ns("download_graph"), icon = shiny::icon("download"), "Télécharger le graphique")
          ),
        shiny::absolutePanel(
          id = "reach-logo",
          #class = "well",
          fixed = TRUE,
          draggable = F,
          top = 1000,
          left = "auto",
          right = 30,
          width = 350,
          shiny::img(src = "www/reach_logo.png", width = "80%", align = "right")
        )
    )
  )
}

#' graph_main Server Functions
#'
#' @noRd
mod_graph_main_server <- function(id){
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
             "National" = HTI.MSNA.2022::data_main_simple |>
               mutate_if_nulla(choices_label, " "),
             "Départemental" = HTI.MSNA.2022::data_admin1_simple |>
               mutate_if_nulla(choices_label, " ")
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




    output$infobox <- shiny::renderUI({

      sector <- input$rq
      sub_sector <- input$sub_rq
      indicator <- input$indicator
      choice <- input$choice

      analysis_filtered <- analysis() |>
        dplyr::filter(sector == rq, sub_sector == sub_rq, choices_label == choice)

      recall <- ifelse(
        is.na(unique(analysis_filtered$recall)),
        "Aucune",
        unique(analysis_filtered$recall)
      )

      subset <-
        ifelse(
          is.na(unique(analysis_filtered$subset)),
          "Aucun",
          unique(analysis_filtered$subset)
        )

      pop_group <- "Population générale"

      info_box(main_title = sector,
               sub_title = sub_sector,
               pop_group = pop_group,
               indicator = indicator,
               recall = recall,
               subset = subset,
               prefix_recall = "Période de rappel :",
               prefix_subset = "Sous-ensemble :")

    })

    output$graph <-
      plotly::renderPlotly(
        expr = {

        if (input$disagg == "National") {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator
            )
        } else {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator,
                          choices_label == input$choice
            )
        }

        analysis_filtered <- analysis_filtered |>
          mutate_if_nulla(stat, 0) |> 
          dplyr::arrange(dplyr::desc(stat)) |>
          dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
          dplyr::mutate(choices_label = dplyr::case_when(
            analysis_name == "Moyenne" ~ "Moyenne",
            analysis_name == "Médiane" ~ "Médiane",
            analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
            TRUE ~ choices_label)
          )
  

       if (input$disagg == "National") {

         if (nrow(analysis_filtered) == 0) {
           graph <- visualizeR::hbar(
             .tbl = tibble::tibble(stat = 100, choices_label = "Missing data"),
             x = stat,
             y = choices_label,
             reverse = TRUE,
             gg_theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 12))
         } else {

           graph <- visualizeR::hbar(
             .tbl = analysis_filtered |>
               dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
             x = stat,
             y = choices_label,
             reverse = TRUE,
             gg_theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 12)) +
             ggplot2::scale_y_discrete(labels = scales::label_wrap(50))
         }

        } else if (input$disagg == "Départemental") {

          if (nrow(analysis_filtered) == 0) {
            graph <- visualizeR::hbar(
              .tbl = tibble::tibble(stat = 100, choices_label = "Missing data"),
              x = stat,
              y = choices_label,
              reverse = TRUE,
              gg_theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 12))
          } else {

            graph <- visualizeR::hbar(
              .tbl = analysis_filtered |>
                dplyr::mutate(group_disagg_label = factor(group_disagg_label, levels = unique(analysis_filtered$group_disagg_label))),
                x = stat,
                y = group_disagg_label,
                reverse = TRUE,
                gg_theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 12))
          }
        }

        graph <- ggplot_to_plotly(
          graph,
          paste0("HTI MSNA 2022 - ", input$disagg, " - ", input$indicator, ifelse(input$disagg != "National", paste0( " - ", input$choice), ""), ".svg"))

        return(graph)
      })

    shiny::observeEvent(input$download_graph, {
      shinyscreenshot::screenshot(id = "graph",
                                  filename = paste0("HTI MSNA 2022 - ", input$disagg, " - ", input$indicator, ifelse(input$disagg != "National", paste0( " - ", input$choice), "")))
    })



  })
}

## To be copied in the UI
# mod_graph_main_ui("graph_main_1")

## To be copied in the server
# mod_graph_main_server("graph_main_1")
