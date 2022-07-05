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
    "Global",
    shiny::sidebarPanel(
      width = 3,
      shiny::fluidRow(
          shiny::selectInput(
            inputId = ns("main_rq"),
            label = "Secteur",
            choices = unique(HTI.MSNA.2022::data_main_simple$rq),
            selected = "EPHA")
        ),
      shiny::fluidRow(
          shiny::selectInput(
            inputId = ns("main_sub_rq"),
            label = "Sous-secteur",
            choices = unique(HTI.MSNA.2022::data_main_simple$sub_rq),
            selected = "Accès à l'eau")
        ),
      shiny::fluidRow(
          shiny::selectInput(
            inputId = ns("main_indicator"),
            label = "Indicateur",
            choices = unique(HTI.MSNA.2022::data_main_simple$indicator),
            selected = "% de ménages par source d'eau de boisson")
        ),
      shiny::fluidRow(
        shiny::img(src = "www/reach_logo.png", width = "80%", align = "center")
      )
      ),
    shiny::mainPanel(
      shiny::h3(shiny::textOutput(ns("main_indicator_name"))),
      reactable::reactableOutput(ns("main_table")))
  )



}

#' indicator_main Server Functions
#'
#' @noRd
mod_indicator_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    analysis_main_simple <- HTI.MSNA.2022::data_main_simple |>
      dplyr::mutate(stat = ifelse(analysis_name == "Proportion", round(stat * 100, 0), round(stat, 1)))

    shiny::observeEvent(input$main_rq, {
      shiny::updateSelectInput(session,
                               "main_sub_rq",
                               choices = analysis_main_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq
                                 ) |>
                                 dplyr::pull(sub_rq) |>
                                 unique()
                               )
    })

    shiny::observeEvent(input$main_sub_rq, {
      shiny::updateSelectInput(session,
                               "main_indicator",
                               choices = analysis_main_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq,
                                   sub_rq == input$main_sub_rq
                                 ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })


    output$main_indicator_name <- shiny::renderText({
      analysis_filtered <- analysis_main_simple |>
        dplyr::filter(indicator == input$main_indicator) |>
        dplyr::pull(indicator) |>
        unique()
    }
    )

    output$main_table <- reactable::renderReactable({
      analysis_filtered <- analysis_main_simple |>
        dplyr::filter(rq == input$main_rq,
                      sub_rq == input$main_sub_rq,
                      indicator == input$main_indicator) |>
        dplyr::select(recall, subset, choices_label, stat) |>
        dplyr::arrange(dplyr::desc(stat))

      rctbl_indicator <- unique(analysis_filtered$indicator)

      analysis_filtered <- analysis_filtered |>
        dplyr::rename("Statistique" = stat, "Option de réponse" = choices_label, "Sous-ensemble" = subset, "Période de rappel" = recall) |>
        janitor::remove_empty(which = "cols") |>
        as.data.frame()

      reactable::reactable(
        analysis_filtered,
        rownames = FALSE,
        bordered = TRUE,
        # striped = TRUE,
        highlight = TRUE,
        # compact = TRUE,
        filterable = TRUE,
        height = "800px",
        class = "reactable",
        style = list(fontFamily = "Leelawadee UI"),
         defaultColDef = colDef(
           cell = reactablefmtr::color_tiles(
             analysis_filtered,
             colors = c("white","#EE5859"),
             box_shadow = TRUE),
           align = "center"))
    })

})}

## To be copied in the UI
# mod_indicator_main_ui("indicator_main_1")

## To be copied in the server
# mod_indicator_main_server("indicator_main_1")






