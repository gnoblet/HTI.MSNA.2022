#' indicator_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_main_ui <- function(id) {
  ns <- NS(id)


  shiny::tabPanel(
    "Tableau",
    icon = shiny::icon("table"),
    value = "panel_table",
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
        # shiny::hr(),
        shiny::p(shiny::htmlOutput(ns("infobox")))
      ),
      shiny::mainPanel(
        fixed = TRUE,
        draggable = FALSE,
        width = 9,
        shiny::h3(shiny::textOutput(ns("indicator_name"))),
        reactable::reactableOutput(ns("table"), width = "auto", height = "80%")
        # shiny::uiOutput(ns("table_ui"))
      )
    ),
    shiny::fluidRow(
      shiny::fixedPanel(
        id = "reach-logo-indicator",
        left = "auto",
        opacity = 0.8,
        right = 30,
        shiny::br(),
        shiny::img(src = "www/reach_logo.png",height = "40px", align = "left")
      )
    )

  )
}

#' indicator_main Server Functions
#'
#' @noRd
mod_indicator_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")



    analysis <- reactive({
      switch(input$disagg,
        "National" =
          switch(input$milieu,
            "Ensemble" = HTI.MSNA.2022::data_overall_all |>
              mutate_if_nulla(choices_label, " ") |>
              mutate_if_nulla(stat, 0) |>
              dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))),
            "Rural et urbain" = HTI.MSNA.2022::data_overall_milieu |>
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
          ),
        "Départemental" =
          switch(input$milieu,
            "Ensemble" = HTI.MSNA.2022::data_overall_admin1 |>
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
              ),
            "Rural et urbain" = HTI.MSNA.2022::data_overall_stratum |>
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

      if (input$disagg == "National" & input$milieu == "Ensemble") {
        filtered <- analysis_filtered |>
          dplyr::select("Type d'analyse" = analysis_name, choices_label, "Statistique" = stat)
      } else if (input$disagg == "Départemental" & input$milieu == "Ensemble" | (input$disagg == "National" & input$milieu == "Rural et urbain")) {
        filtered <- analysis_filtered |>
          impactR::deselect(id_analysis, rq, sub_rq, choices, recall, subset, indicator, analysis_name)
      } else if (input$disagg == "Départemental" & input$milieu == "Rural et urbain") {
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

      if (input$disagg == "Départemental" & input$milieu == "Rural et urbain") {

        rctbl <- rctbl |>
          reactable::reactable(
            class = "reactable",
            rownames = FALSE,
            defaultPageSize = 50,
            bordered = TRUE,
            striped = TRUE,
            highlight = TRUE,
            columns = list(
              "Option de réponse" = reactable::colDef(minWidth = 150, maxWidth = 350, sticky = "left"),
              "Artibonite - Urbain" = reactable::colDef(name = "Urbain"),
              "Artibonite - Rural" = reactable::colDef(name = "Rural"),
              "Centre - Urbain" = reactable::colDef(name = "Urbain"),
              "Centre - Rural" = reactable::colDef(name = "Rural"),
              "Grand'Anse - Urbain" = reactable::colDef(name = "Urbain"),
              "Grand'Anse - Rural" = reactable::colDef(name = "Rural"),
              "Nippes - Urbain" = reactable::colDef(name = "Urbain"),
              "Nippes - Rural" = reactable::colDef(name = "Rural"),
              "Nord - Urbain" = reactable::colDef(name = "Urbain"),
              "Nord - Rural" = reactable::colDef(name = "Rural"),
              "Nord Est - Urbain" = reactable::colDef(name = "Urbain"),
              "Nord Est - Rural" = reactable::colDef(name = "Rural"),
              "Nord Ouest - Urbain" = reactable::colDef(name = "Urbain"),
              "Nord Ouest - Rural" = reactable::colDef(name = "Rural"),
              "Ouest - Urbain" = reactable::colDef(name = "Urbain"),
              "Ouest - Rural" = reactable::colDef(name = "Rural"),
              "Sud - Urbain" = reactable::colDef(name = "Urbain"),
              "Sud - Rural" = reactable::colDef(name = "Rural"),
              "Sud Est - Urbain" = reactable::colDef(name = "Urbain"),
              "Sud Est - Rural" = reactable::colDef(name = "Rural")
            ),
            columnGroups = list(
              colGroup(name = "Artibonite", columns = c("Artibonite - Rural", "Artibonite - Urbain")),
              colGroup(name = "Centre", columns = c("Centre - Rural", "Centre - Urbain")),
              colGroup(name = "Grand'Anse", columns = c("Grand'Anse - Rural", "Grand'Anse - Urbain")),
              colGroup(name = "Ouest", columns = c("Ouest - Rural", "Ouest - Urbain")),
              colGroup(name = "Nippes", columns = c("Nippes - Rural", "Nippes - Urbain")),
              colGroup(name = "Nord", columns = c("Nord - Rural", "Nord - Urbain")),
              colGroup(name = "Nord Est", columns = c("Nord Est - Rural", "Nord Est - Urbain")),
              colGroup(name = "Nord Ouest", columns = c("Nord Ouest - Rural", "Nord Ouest - Urbain")),
              colGroup(name = "Sud", columns = c("Sud - Rural", "Sud - Urbain")),
              colGroup(name = "Sud Est", columns = c("Sud Est - Rural", "Sud Est - Urbain"))
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
# mod_indicator_main_ui("indicator_main_1")

## To be copied in the server
# mod_indicator_main_server("indicator_main_1")
