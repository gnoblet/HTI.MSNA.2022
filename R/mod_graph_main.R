#' graph_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_main_ui <- function(id) {
  ns <- NS(id)

  shiny::tabPanel(
    title = "Diagramme",
    value = "panel-graph",
      icon = shiny::icon("chart-bar"),
    shiny::fluidRow(
      shiny::sidebarPanel(
        width = 3,
        class = "well",
        shinyWidgets::prettyRadioButtons(
          inputId = ns("disagg"),
          label = "Niveau géographique",
          choices = c("National", "Départemental"),
          selected = "National",
          fill = TRUE,
          status = "danger"
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("milieu"),
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
        shiny::conditionalPanel(
          condition = "input.disagg == 'Départemental' || input.disagg == 'Départemental et milieu'",
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
        # shiny::h3(shiny::textOutput(ns("indicator_name"))),
        shiny::plotOutput(ns("graph"), width = "80%", height = "700px") #
      )
    ),
    shiny::fluidRow(
      shiny::fixedPanel(
        id = "reach-logo-graph",
        # fixed = TRUE,
        opacity = 0.8,
        draggable = F,
        # bottom = "7%",
        # top = "93%",
        left = "auto",
        right = 30,
        # width = 400,

        shiny::img(src = "www/reach_logo.png", height = "40px", align = "left")
      )
    )
  )

  # actionButton(ns("download_graph"), icon = shiny::icon("download"), "Télécharger le graphique")
}

#' graph_main Server Functions
#'
#' @noRd
mod_graph_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {

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
             "National" =
               switch(input$milieu,
                      "Ensemble" = HTI.MSNA.2022::data_overall_all |>
                        mutate_if_nulla(choices_label, " ") |>
                        mutate_if_nulla(stat, 0) |>
                        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
                        dplyr::mutate(choices_label = dplyr::case_when(
                          analysis_name == "Moyenne" ~ "Moyenne",
                          analysis_name == "Médiane" ~ "Médiane",
                          analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                          TRUE ~ choices_label)),
                      "Rural et urbain" = HTI.MSNA.2022::data_overall_milieu |>
                        mutate_if_nulla(choices_label, " ") |>
                        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
                        dplyr::mutate(choices_label = dplyr::case_when(
                          analysis_name == "Moyenne" ~ "Moyenne",
                          analysis_name == "Médiane" ~ "Médiane",
                          analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                          TRUE ~ choices_label))
                      ),
             "Départemental" =
               switch(input$milieu,
                      "Ensemble" = HTI.MSNA.2022::data_overall_admin1 |>
                        mutate_if_nulla(choices_label, " ") |>
                        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
                        dplyr::mutate(choices_label = dplyr::case_when(
                          analysis_name == "Moyenne" ~ "Moyenne",
                          analysis_name == "Médiane" ~ "Médiane",
                          analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                          TRUE ~ choices_label
                        )),
                      "Rural et urbain" =  HTI.MSNA.2022::data_overall_stratum |>
                        mutate_if_nulla(choices_label, " ") |>
                        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))) |>
                        dplyr::mutate(choices_label = dplyr::case_when(
                          analysis_name == "Moyenne" ~ "Moyenne",
                          analysis_name == "Médiane" ~ "Médiane",
                          analysis_name == "Proportion" & analysis == "ratio" ~ "Proportion (%)",
                          TRUE ~ choices_label
                        ))
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
          if (input$disagg == "National" & input$milieu == "Ensemble") {

            analysis_filtered <- analysis() |>
              dplyr::filter(
                rq == input$rq,
                sub_rq == input$sub_rq,
                indicator == input$indicator
              ) |>
              mutate_if_nulla(stat, 0) |>
              dplyr::arrange(dplyr::desc(stat))

          } else if (input$disagg == "Départemental" & input$milieu == "Ensemble") {

            analysis_filtered <- analysis() |>
              dplyr::filter(
                rq == input$rq,
                sub_rq == input$sub_rq,
                indicator == input$indicator,
                choices_label == input$choice
              )

            missing_admin1 <- admin1_f() |>
              dplyr::filter(!(admin1 %in% analysis_filtered$group_disagg)) |>
              dplyr::rename(group_disagg = admin1, group_disagg_label = admin1_name)

            analysis_filtered <- analysis_filtered |>
              dplyr::bind_rows(missing_admin1) |>
              mutate_if_nulla(stat, 0) |>
              dplyr::arrange(dplyr::desc(stat))

          } else if (input$disagg == "National" & input$milieu == "Rural et urbain") {

            analysis_filtered <- analysis() |>
              dplyr::filter(
                rq == input$rq,
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

          } else if (input$disagg == "Départemental" & input$milieu == "Rural et urbain") {

            analysis_filtered <- analysis() |>
              dplyr::filter(
                rq == input$rq,
                sub_rq == input$sub_rq,
                indicator == input$indicator,
                choices_label == input$choice
              )

            missing_stratum <- stratum_f() |>
              dplyr::filter(!(stratum %in% analysis_filtered$group_disagg)) |>
              dplyr::rename(group_disagg = stratum, group_disagg_label = stratum_name)

            analysis_filtered <- analysis_filtered |>
              dplyr::bind_rows(missing_stratum) |>
              mutate_if_nulla(stat, 0) |>
              dplyr::arrange(dplyr::desc(stat)) |>
              dplyr::mutate(
                milieu = ifelse(stringr::str_detect(group_disagg, "_urbain"), "Urbain", "Rural"),
                admin1 = stringr::str_remove_all(group_disagg_label, " -.*")
              )

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
                title_size = 14,
                bg_plot_pal = "#FFFFFF",
                bg_panel_pal = "#FFFFFF",
                grid_v = TRUE
              )
            )
          } else {
            if (input$disagg == "National" & input$milieu == "Ensemble") {

              graph <- visualizeR::hbar(
                .tbl = analysis_filtered |>
                  dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
                x = stat,
                y = choices_label,
                reverse = TRUE,
                gg_theme = ggblanket::gg_theme(
                  font = "Leelawadee UI",
                  body_size = 10,
                  title_size = 14,
                  subtitle_size = 13,
                  bg_plot_pal = "#FFFFFF",
                  bg_panel_pal = "#FFFFFF",
                  grid_v = TRUE
                )) +
                  ggplot2::geom_text(
                    ggplot2::aes(
                      label = stat,
                      group = group_disagg_label),
                    hjust = 1.5,
                    colour = "white",
                    fontface = "bold",
                    position = ggplot2::position_dodge(width = 0.8))
                ggplot2::scale_y_discrete(labels = scales::label_wrap(70))

            } else if (input$disagg == "Départemental" & input$milieu == "Ensemble") {
              graph <- visualizeR::hbar(
                .tbl = analysis_filtered |>
                  dplyr::mutate(group_disagg_label = factor(group_disagg_label, levels = unique(analysis_filtered$group_disagg_label))),
                x = stat,
                y = group_disagg_label,
                width = 0.4,
                reverse = TRUE,
                position = ggplot2::position_dodge(width = 0.2),
                gg_theme = ggblanket::gg_theme(
                  font = "Leelawadee UI",
                  body_size = 10,
                  title_size = 14,
                  subtitle_size = 13,
                  bg_plot_pal = "#FFFFFF",
                  bg_panel_pal = "#FFFFFF",
                  grid_v = TRUE
                )
              ) +
                ggplot2::geom_text(
                  ggplot2::aes(
                    label = stat),
                  hjust = 1.5,
                  colour = "white",
                  fontface = "bold")
            } else if (input$disagg == "National" & input$milieu == "Rural et urbain") {
              graph <- visualizeR::hbar(
                .tbl = analysis_filtered |>
                  dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
                x = stat,
                y = choices_label,
                group = group_disagg_label,
                group_title = "Milieu",
                width = 0.8,
                reverse = TRUE,
                gg_theme = ggblanket::gg_theme(
                  font = "Leelawadee UI",
                  body_size = 10,
                  title_size = 14,
                  subtitle_size = 13,
                  bg_plot_pal = "#FFFFFF",
                  bg_panel_pal = "#FFFFFF",
                  grid_v = TRUE
                )) +
                  ggplot2::geom_text(
                    ggplot2::aes(
                      label = stat,
                      group = group_disagg_label),
                    hjust = 1.5,
                    colour = "white",
                    fontface = "bold",
                    position = ggplot2::position_dodge(width = 0.8)) +
                ggplot2::scale_y_discrete(labels = scales::label_wrap(70))

            } else if (input$disagg == "Départemental" & input$milieu == "Rural et urbain") {
              graph <- visualizeR::hbar(
                .tbl = analysis_filtered |>
                  dplyr::mutate(admin1 = factor(admin1, levels = unique(analysis_filtered$admin1))) |>
                  ggblanket::add_tooltip_text(stat, admin1, milieu),
                x = stat,
                y = admin1,
                group = milieu,
                group_title = "Milieu",
                reverse = TRUE,
                gg_theme = ggblanket::gg_theme(
                  font = "Leelawadee UI",
                  body_size = 11,
                  title_size = 16,
                  subtitle_size = 15,
                  bg_plot_pal = "#FFFFFF",
                  bg_panel_pal = "#FFFFFF",
                  grid_v = TRUE
                ),
                width = 0.8
              ) +
                ggplot2::geom_text(
                  ggplot2::aes(
                    label = stat,
                    group = milieu),
                  hjust = 1.5,
                  colour = "white",
                  fontface = "bold",
                  position = ggplot2::position_dodge(width = 0.8))

            }
          }

          if (input$disagg == "Départemental")  graph <- graph + ggplot2::ggtitle(indicator_name(), subtitle = paste("Option de réponse :",  choice_name(), sep = " "))

          if (input$disagg == "National")  graph <- graph + ggplot2::ggtitle(indicator_name())

          graph <- graph +
            ggplot2::theme(
              legend.position = ifelse(input$milieu == "Rural et urbain", "right", "none"),
              legend.direction = "vertical",
              plot.title = ggplot2::element_text(vjust = 3))
            # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))
            # ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()) +


          return(graph)
        }
      )

    shiny::observeEvent(input$download_graph, {
      shinyscreenshot::screenshot(
        id = "graph",
        filename = paste0("HTI MSNA 2022 - ", input$disagg, " - ", input$indicator, ifelse(input$disagg != "National", paste0(" - ", input$choice), ""))
      )
    })
  })
}

## To be copied in the UI
# mod_graph_main_ui("graph_main_1")

## To be copied in the server
# mod_graph_main_server("graph_main_1")
