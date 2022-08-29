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
    shiny::sidebarPanel(
      width = 3,
      class = "well",
      shinyWidgets::prettyRadioButtons(
        inputId = ns("disagg"),
        label = "Niveau géographique",
        choices = c("National", "Départemental", "Milieu", "Départemental et milieu"),
        selected = "National",
        fill = TRUE,
        status = "danger"
      ),
      shiny::selectInput(
        inputId = ns("rq"),
        label = "Secteur",
        choices = c("Information générale", "Démographie du ménage", "Déplacement", "Washington Group", "Santé", "Education", "Sécurité alimentaire", "Moyens de subsistance", "ABNA", "EPHA", "Protection", "Redevabilité"),
        selected = "EPHA"
      ),
      shiny::selectInput(
        inputId = ns("sub_rq"),
        label = "Sous-secteur",
        choices = "Accès à l'eau",
        selected = "Accès à l'eau"
      ),
      shiny::selectInput(
        inputId = ns("indicator"),
        label = "Indicateur",
        choices = "% de ménages par source d'eau de boisson",
        selected = "% de ménages par source d'eau de boisson"
      ),
      shiny::hr(),
      shiny::p(shiny::htmlOutput(ns("infobox")))
    ),
    shiny::mainPanel(
      fixed = TRUE,
      draggable = FALSE,
      width = 9,
      shiny::h3(shiny::textOutput(ns("indicator_name"))),
      reactable::reactableOutput(ns("table"), width = "auto", height = "auto")
      # shiny::uiOutput(ns("table_ui"))
    ),
    shiny::absolutePanel(
      id = "reach-logo",
      fixed = TRUE,
      draggable = F,
      bottom = "7%",
      top = "93%",
      left = 30,
      right = "auto",
      width = 400,
      shiny::img(src = "www/reach_logo.png", height = "60px", align = "left")
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
        "National" = HTI.MSNA.2022::data_main |>
          mutate_if_nulla(choices_label, " ") |>
          mutate_if_nulla(stat, 0) |>
          dplyr::arrange(dplyr::desc(stat)) |>
          dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1))),
        "Départemental" = HTI.MSNA.2022::data_admin1 |>
          mutate_if_nulla(choices_label, " ") |>
          dplyr::distinct(id_analysis, choices_label, group_disagg_label, .keep_all = T) |>
          tidyr::pivot_wider(
            id_cols = c(id_analysis, analysis_name, rq, sub_rq, indicator, choices, recall, subset, choices_label),
            names_from = group_disagg_label,
            values_from = stat
          ) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(analysis_name == "Proportion", round(x * 100, 0), round(x, 1)))),
        "Milieu" = HTI.MSNA.2022::data_milieu |>
          mutate_if_nulla(choices_label, " ") |>
          dplyr::distinct(id_analysis, choices_label, group_disagg_label, .keep_all = T) |>
          tidyr::pivot_wider(
            id_cols = c(id_analysis, analysis_name, rq, sub_rq, indicator, choices, recall, subset, choices_label),
            names_from = group_disagg_label,
            values_from = stat
          ) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(analysis_name == "Proportion", round(x * 100, 0), round(x, 1)))),
        "Départemental et milieu" = HTI.MSNA.2022::data_stratum |>
          mutate_if_nulla(choices_label, " ") |>
          dplyr::distinct(id_analysis, choices_label, group_disagg_label, .keep_all = T) |>
          tidyr::pivot_wider(
            id_cols = c(id_analysis, analysis_name, rq, sub_rq, indicator, choices, recall, subset, choices_label),
            names_from = group_disagg_label,
            values_from = stat
          ) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))) |>
          dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(analysis_name == "Proportion", round(x * 100, 0), round(x, 1))))
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

      analysis_filtered <- analysis() |>
        dplyr::filter(sector == rq, sub_sector == sub_rq, indicator == indicator)

      recall <- ifelse(
        length(unique(na.omit(analysis_filtered$recall))) == 0,
        "Aucune",
        unique(na.omit(analysis_filtered$recall))
      )

      subset <-
        ifelse(
          length(unique(na.omit(analysis_filtered$subset))) == 0,
          "Aucun",
          unique(na.omit(analysis_filtered$subset))
        )

      pop_group <- "Population générale"

      reduced_info_box(
        pop_group = pop_group,
        recall = recall,
        subset = subset,
        prefix_pop_group = "Groupe de population : ",
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

      if (input$disagg == "National") {
        filtered <- analysis_filtered |>
          dplyr::select(choices_label, "Statistique" = stat)
      } else if (input$disagg %in% c("Départemental", "Milieu", "Départemental et milieu")) {
        filtered <- analysis_filtered |>
          impactR::deselect(id_analysis, rq, sub_rq, choices, recall, subset, indicator, analysis_name)
      }

      rctbl <- filtered |>
        dplyr::rename("Option de réponse" = choices_label) |>
        dplyr::mutate(dplyr::across(where(is.character), \(x) ifelse(x == " ", NA_character_, x))) |>
        janitor::remove_empty(which = "cols") |>
        as.data.frame() |>
        reactable::reactable(
          class = "reactable",
          rownames = FALSE,
          defaultPageSize = 50,
          bordered = TRUE,
          striped = TRUE,
          highlight = TRUE,
          columns = list(
            "Option de réponse" = reactable::colDef(minWidth = 150, maxWidth = 350)
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
