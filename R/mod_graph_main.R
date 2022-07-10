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
    "Graph - Global",
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
      shiny::h3(shiny::div("Indicateur : ", shiny::textOutput(ns("main_indicator_name")))),
      # shiny::h3("Choix : ", shiny::textOutput(ns("main_choices_label"))),
      shiny::plotOutput(ns("main_graph"))
      # shiny::dataTableOutput(ns("main_graph")))
    )

  )
}

#' graph_main Server Functions
#'
#' @noRd
mod_graph_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    analysis_admin1_simple <- HTI.MSNA.2022::data_admin1_simple |>
      dplyr::mutate(stat = ifelse(analysis_name == "Proportion" & analysis != "ratio", round(stat * 100, 0), round(stat, 1)))

    shiny::observeEvent(input$main_rq, {
      shiny::updateSelectInput(session,
                               "main_sub_rq",
                               choices = analysis_admin1_simple |>
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
                               choices = analysis_admin1_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq,
                                   sub_rq == input$main_sub_rq
                                 ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })


    output$main_indicator_name <- shiny::renderText({
      analysis_filtered <- analysis_admin1_simple |>
        dplyr::filter(indicator == input$main_indicator) |>
        dplyr::pull(indicator) |>
        unique()
    })


    output$main_graph <-
      # shiny::renderDataTable({
      shiny::renderPlot({
        analysis_filtered <- analysis_admin1_simple |>
          dplyr::filter(rq == input$main_rq,
                        sub_rq == input$main_sub_rq,
                        indicator == input$main_indicator) |>
          dplyr::mutate(stat = ifelse(is.na(stat), 0, stat)) |>
          dplyr::arrange(dplyr::desc(stat)) |>
          visualizeR::hbar(x = stat,
                           y = choices_label,
                           font_family = "Leelawadee UI",
                           reverse = TRUE)

        analysis_filtered
      })

  })
}

## To be copied in the UI
# mod_graph_main_ui("graph_main_1")

## To be copied in the server
# mod_graph_main_server("graph_main_1")
