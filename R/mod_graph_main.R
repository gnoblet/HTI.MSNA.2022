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
    title = "Graphes",
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
          shiny::h3(shiny::textOutput(ns("indicator_name"))),
          plotly::plotlyOutput(ns("graph"), height = "750px")
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
            choices = c("National", "Milieu", "Départemental", "Départemental et milieu"),
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
            condition = "input.disagg == 'Départemental' || input.disagg == 'Départemental et milieu'",
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
             "National" = HTI.MSNA.2022::data_main |>
              mutate_if_nulla(choices_label, " ") |>
              mutate_if_nulla(stat, 0) |>
              dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
              dplyr::mutate(choices_label = dplyr::case_when(
                analysis_name == "Moyenne" ~ "Moyenne",
                analysis_name == "Médiane" ~ "Médiane",
                analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                TRUE ~ choices_label)),
             "Départemental" = HTI.MSNA.2022::data_admin1 |>
              mutate_if_nulla(choices_label, " ") |>
              dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
              dplyr::mutate(choices_label = dplyr::case_when(
                analysis_name == "Moyenne" ~ "Moyenne",
                analysis_name == "Médiane" ~ "Médiane",
                analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                TRUE ~ choices_label)
                ),
             "Milieu" = HTI.MSNA.2022::data_milieu |>
               mutate_if_nulla(choices_label, " ") |>
               dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
               dplyr::mutate(choices_label = dplyr::case_when(
                 analysis_name == "Moyenne" ~ "Moyenne",
                 analysis_name == "Médiane" ~ "Médiane",
                 analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                 TRUE ~ choices_label)
                 ),
             "Départemental et milieu" = HTI.MSNA.2022::data_stratum |>
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


    indicator_name <- shiny::reactive({
      input$indicator
    })

    output$indicator_name <- shiny::renderText(indicator_name())


    output$graph <-
      plotly::renderPlotly(
        expr = {

        if (input$disagg == "National") {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator
            ) |>
            mutate_if_nulla(stat, 0) |>
            dplyr::arrange(dplyr::desc(stat))
        } else if (input$disagg == "Départemental") {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator,
                          choices_label == input$choice
            )

          missing_admin1 <- admin1_f() |>
            dplyr::filter(!admin1 %in% c("ouest")) |>
            dplyr::filter(!(admin1 %in% analysis_filtered$group_disagg)) |>
            dplyr::rename(group_disagg = admin1, group_disagg_label = admin1_name)

          analysis_filtered <- analysis_filtered |>
            dplyr::bind_rows(missing_admin1) |>
            mutate_if_nulla(stat, 0) |>
            dplyr::arrange(dplyr::desc(stat))

        } else if (input$disagg == "Milieu") {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator
            )

          missing_milieu <- milieu_f() |>
            dplyr::filter(!(milieu %in% analysis_filtered$group_disagg)) |>
            dplyr::rename(group_disagg = milieu, group_disagg_label = milieu_name)

          analysis_filtered <- analysis_filtered |>
            dplyr::bind_rows(missing_milieu) |>
            mutate_if_nulla(stat, 0) |>
            dplyr::arrange(dplyr::desc(stat))

        } else if (input$disagg == "Départemental et milieu") {
          analysis_filtered <- analysis() |>
            dplyr::filter(rq == input$rq,
                          sub_rq == input$sub_rq,
                          indicator == input$indicator,
                          choices_label == input$choice
            )

          missing_stratum <- stratum_f() |>
            dplyr::filter(!stratum %in% c("ouest_urbain", "ouest_rural")) |>
            dplyr::filter(!(stratum %in% analysis_filtered$group_disagg)) |>
            dplyr::rename(group_disagg = stratum, group_disagg_label = stratum_name)

          analysis_filtered <- analysis_filtered |>
            dplyr::bind_rows(missing_stratum) |>
            mutate_if_nulla(stat, 0) |>
            dplyr::arrange(dplyr::desc(stat)) |>
            dplyr::mutate(
              milieu = ifelse(stringr::str_detect(group_disagg, "_urbain"), "Urbain", "Rural"),
              admin1 = stringr::str_remove_all(group_disagg_label, " -.*"))

        }


        if (nrow(analysis_filtered) == 0) {
          graph <- visualizeR::hbar(
            .tbl = tibble::tibble(stat = 100, choices_label = "Missing data"),
            x = stat,
            y = choices_label,
            reverse = TRUE,
            gg_theme = ggblanket::gg_theme(
              font = "Leelawadee UI",
              body_size = 10,
              bg_plot_pal = "#FFFFFF",
              bg_panel_pal = "#FFFFFF",
              grid_v = TRUE))

        } else {

          if (input$disagg == "National") {


            graph <- visualizeR::hbar(
              .tbl = analysis_filtered |>
                dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
              x = stat,
              y = choices_label,
              reverse = TRUE,
              gg_theme = ggblanket::gg_theme(
                font = "Leelawadee UI",
                body_size = 10,
                bg_plot_pal = "#FFFFFF",
                bg_panel_pal = "#FFFFFF",
                grid_v = TRUE)) +
              ggplot2::scale_y_discrete(labels = scales::label_wrap(70))

          } else if (input$disagg == "Départemental") {

            graph <- visualizeR::hbar(
              .tbl = analysis_filtered |>
                dplyr::mutate(group_disagg_label = factor(group_disagg_label, levels = unique(analysis_filtered$group_disagg_label))),
              x = stat,
              y = group_disagg_label,
              reverse = TRUE,
              gg_theme = ggblanket::gg_theme(
                font = "Leelawadee UI",
                body_size = 10,
                bg_plot_pal = "#FFFFFF",
                bg_panel_pal = "#FFFFFF",
                grid_v = TRUE))

          } else if (input$disagg == "Milieu") {

            graph <- visualizeR::hbar(
              .tbl = analysis_filtered |>
                dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
              x = stat,
              y = choices_label,
              group = group_disagg_label,
              group_title = "Milieu",
              reverse = TRUE,
              gg_theme = ggblanket::gg_theme(
                font = "Leelawadee UI",
                body_size = 10,
                bg_plot_pal = "#FFFFFF",
                bg_panel_pal = "#FFFFFF",
                grid_v = TRUE)) +
              ggplot2::scale_y_discrete(labels = scales::label_wrap(70))

          } else if (input$disagg == "Départemental et milieu") {

            graph <- visualizeR::hbar(
              .tbl = analysis_filtered |>
                dplyr::mutate(admin1 = factor(admin1, levels = unique(analysis_filtered$admin1))),
              x = stat,
              y = admin1,
              group = milieu,
              group_title = "Milieu",
              reverse = TRUE,
              gg_theme = ggblanket::gg_theme(
                font = "Leelawadee UI",
                body_size = 10,
                bg_plot_pal = "#FFFFFF",
                bg_panel_pal = "#FFFFFF",
                grid_v = TRUE))

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
