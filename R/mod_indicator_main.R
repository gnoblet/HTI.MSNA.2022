#' indicator_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_main_ui <- function(id){
  ns <- NS(id)


  shiny::tabPanel(
    "Tableaux",
    icon = shiny::icon("table"),
    value = "panel_table",
    div(class="outer",

        shiny::absolutePanel(
          fixed = TRUE,
          draggable = FALSE,
          top = 90,
          left = 400,
          right = "auto",
          width = 1000,
          shiny::h3(shiny::textOutput(ns("indicator_name"))),
          reactable::reactableOutput(ns("table")))
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
      shiny::img(src = "www/reach_logo.png", width = "80%", align = "center")
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
      actionButton(ns("download_table"), icon = shiny::icon("download"), "Télécharger la table")
    )
    )



}

#' indicator_main Server Functions
#'
#' @noRd
mod_indicator_main_server <- function(id){
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
             "National" = HTI.MSNA.2022::data_main_simple |>
               dplyr::mutate(choices_label = ifelse(is.na(choices_label), " ", choices_label)),
             "Départemental" = HTI.MSNA.2022::data_admin1_simple |>
               dplyr::mutate(choices_label = ifelse(is.na(choices_label), " ", choices_label)))
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
        dplyr::filter(sector == rq, sub_sector == sub_rq)

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

      shiny::HTML(sprintf("
                          <span style = 'font-size: 26px; color: %s; font-weight: bold; line-height: 1.2;'> %s </span>
                          <br>
                          <span style = 'font-size: 22px; color: %s; font-weight: bold;line-height: 1.2;'> %s </span>
                          <br>
                          <span style = 'font-size: 18px; color: %s; font-weight: bold;line-height: 1.2;'> %s </span>
                          <hr>
                          <span style = 'font-size: 16px; color: %s; font-weight: bold;'> %s </span>
                          <hr>
                          <span style = 'font-size: 16px; color: %s;'> <strong> Période de rappel : </strong> %s </span>
                          <br>
                          <span style = 'font-size: 16px; color: %s;'> <strong> Sous-ensemble : </strong> %s </span>
                          ",
                          main_red,
                          sector,
                          main_red,
                          sub_sector,
                          white,
                          pop_group,
                          white,
                          indicator,
                          white,
                          recall,
                          white,
                          subset))
    })




    # output$indicator_name <- shiny::renderText({
    #   analysis() |>
    #     dplyr::filter(indicator == input$indicator) |>
    #     dplyr::pull(indicator) |>
    #     unique()
    # })


    output$table <- reactable::renderReactable({

      analysis_filtered <- analysis() |>
        dplyr::filter(rq == input$rq,
                      sub_rq == input$sub_rq,
                      indicator == input$indicator
        ) |>
        dplyr::mutate(stat = ifelse(is.na(stat), 0, stat)) |>
        dplyr::arrange(dplyr::desc(stat)) |>
        dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)))

      if (input$disagg == "National") {

        rctbl <- analysis_filtered |>
          dplyr::select(recall, subset, choices_label, "Statistique" = stat)

      } else if (input$disagg == "Départemental") {

        rctbl <- analysis_filtered |>
          tidyr::pivot_wider(
            id_cols = c(id_analysis, indicator, choices, recall, subset, choices_label),
            names_from = group_disagg_label,
            values_from = stat) |>
          impactR::deselect(id_analysis, choices, indicator)
      }

      rctbl <- rctbl |>
        dplyr::rename("Option de réponse" = choices_label, "Sous-ensemble" = subset, "Période de rappel" = recall) |>
        janitor::remove_empty(which = "cols") |>
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))) |>
        as.data.frame() |>
        reactable::reactable(
          class = "reactable",
          rownames = FALSE,
          defaultPageSize = 50,
          bordered = TRUE,
          # striped = TRUE,
          highlight = TRUE,
          # compact = TRUE,
          filterable = TRUE,
          height = "800px",
          columns = list(
            "Option de réponse" = reactable::colDef(minWidth = 250, maxWidth = 350)
          ),
          defaultColDef = reactable::colDef(
            vAlign = "center",
            maxWidth = 150,
            cell = reactablefmtr::color_tiles(
              analysis_filtered,
              colors = c("white","#EE5859"),
              box_shadow = TRUE,
              span = TRUE),
            align = "center"))

      return(rctbl)
    })

    shiny::observeEvent(input$download_table, {
      shinyscreenshot::screenshot(id = "table",
                                  filename = paste0("HTI MSNA 2022 - ", input$disagg, " - ", input$indicator))
    })


})}

## To be copied in the UI
# mod_indicator_main_ui("indicator_main_1")

## To be copied in the server
# mod_indicator_main_server("indicator_main_1")






