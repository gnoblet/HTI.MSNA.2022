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
    shiny::sidebarPanel(
      width = 3,
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
    shiny::mainPanel(
      shiny::h3(shiny::textOutput(ns("indicator_name"))),
      reactable::reactableOutput(ns("table")))
  )



}

#' indicator_main Server Functions
#'
#' @noRd
mod_indicator_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    analysis <- reactive({
      switch(input$disagg,
             "National" = HTI.MSNA.2022::data_main_simple,
             "Départemental" = HTI.MSNA.2022::data_admin1_simple)
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




    output$indicator_name <- shiny::renderText({
      analysis() |>
        dplyr::filter(indicator == input$indicator) |>
        dplyr::pull(indicator) |>
        unique()
    })


    output$table <- reactable::renderReactable({

      analysis_filtered <- analysis() |>
        dplyr::filter(rq == input$rq,
                      sub_rq == input$sub_rq,
                      indicator == input$indicator
        ) |>
        dplyr::mutate(stat = ifelse(is.na(stat), 0, stat)) |>
        dplyr::arrange(dplyr::desc(stat)) |>
        dplyr::mutate(stat = ifelse(analysis_name == "Proportion" & analysis != "ratio", round(stat * 100, 0), round(stat, 1)))

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




})}

## To be copied in the UI
# mod_indicator_main_ui("indicator_main_1")

## To be copied in the server
# mod_indicator_main_server("indicator_main_1")






