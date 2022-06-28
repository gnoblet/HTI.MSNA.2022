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

  shiny::tabPanel("Quelques indicateurs",
                  # shiny::fluidRow(
                  #   shinyWidgets::pickerInput(
                  #     inputId = ns("main_rq"),
                  #     label = "Secteur",
                  #     choices = unique(HTI.MSNA.2022::data_main_simple$rq),
                  #     selected = "EPHA",
                  #     options = list(
                  #       `actions-box` = TRUE,
                  #       `deselect-all-text` = "Aucun",
                  #       `select-all-text` = "Tous"),
                  #     multiple = TRUE
                  #   ),
                  #   shinyWidgets::pickerInput(
                  #     inputId = ns("main_sub_rq"),
                  #     label = "Sous-secteur",
                  #     choices = unique(HTI.MSNA.2022::data_main_simple$sub_rq),
                  #     selected = "Accès à l'eau",
                  #     options = list(
                  #       `actions-box` = TRUE,
                  #       `deselect-all-text` = "Aucun",
                  #       `select-all-text` = "Tous"),
                  #     multiple = TRUE
                  #   ),
                  #   shinyWidgets::pickerInput(
                  #     inputId = ns("main_indicator"),
                  #     label = "Indicateur",
                  #     choices = unique(HTI.MSNA.2022::data_main_simple$indicator),
                  #     selected = "% de ménages par source d'eau de boisson",
                  #     options = list(
                  #       `actions-box` = TRUE,
                  #       `deselect-all-text` = "Aucun",
                  #       `select-all-text` = "Tous"),
                  #     multiple = TRUE
                  #   )
                  # ),
                  shiny::fluidRow(
                    shiny::selectInput(
                      inputId = ns("main_rq"),
                      label = "Secteur",
                      choices = unique(HTI.MSNA.2022::data_main_simple$rq),
                      selected = "EPHA"
                    ),
                    shiny::selectInput(
                      inputId = ns("main_sub_rq"),
                      label = "Sous-secteur",
                      choices = unique(HTI.MSNA.2022::data_main_simple$sub_rq),
                      selected = "Accès à l'eau"
                    ),
                    shiny::selectInput(
                      inputId = ns("main_indicator"),
                      label = "Indicateur",
                      choices = unique(HTI.MSNA.2022::data_main_simple$indicator),
                      selected = "% de ménages par source d'eau de boisson"
                    )
                  ),

                  shiny::fluidRow((reactable::reactableOutput(ns("main_table"))
                  )
                  )
  )
}

#' indicator_main Server Functions
#'
#' @noRd
mod_indicator_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    analysis_main_simple <- HTI.MSNA.2022::data_main_simple


    shiny::observeEvent(input$main_rq, {
      shiny::req(input$rq)
      shiny::updateSelectInput(session,
                               ns("main_sub_rq"),
                               choices = analysis_main_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq
                                 ) |>
                                 dplyr::pull(sub_rq) |>
                                 unique()
                               )
    })

    shiny::observeEvent(input$main_sub_rq, {
      shiny::req(input$rq, input$sub_rq)
      shiny::updateSelectInput(session,
                               ns("main_indicator"),
                               choices = analysis_main_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq,
                                   sub_rq == input$sub_rq
                                 ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })


})}

## To be copied in the UI
# mod_indicator_main_ui("indicator_main_1")

## To be copied in the server
# mod_indicator_main_server("indicator_main_1")




# output$main_table <- reactable::renderReactable({
#   dt <- analysis_filtered() |>
#     dplyr::select(rq, sub_rq, indicator, analysis_name, choices_label, stat, group_name) |>
#     dplyr::arrange(dplyr::desc(stat)) |>
#     # janitor::remove_empty(which = "cols") |>
#     as.data.frame()
#
#   reactbl <- reactable::reactable(dt,
#                                   rownames = F,
#                                   bordered = TRUE,
#                                   striped = TRUE,
#                                   highlight = TRUE,
#                                   compact = T,
#                                   filterable = T,
#                                   height = "700px",
#                                   style = list(fontFamily = "Leelawadee UI"),
#                                   defaultColDef = colDef(
#                                     cell = reactablefmtr::color_tiles(
#                                       dt,
#                                       colors = c("white","#EE5859"),
#                                       span = TRUE),
#                                     align = "center")
#   )
#
#   return(reactbl)
# })

