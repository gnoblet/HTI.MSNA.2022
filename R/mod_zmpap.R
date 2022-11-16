#' zmpap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zmpap_ui <- function(id){
  ns <- NS(id)
      shiny::tabPanel(
        title = " ZMPAP",
        icon = shiny::icon("city"),
        shiny::tabsetPanel(
          shiny::tabPanel(
            icon = shiny::icon("flag"),
            title = "Introduction",
            mod_zmpap_welcome_ui(ns("zmpap_welcome_1"))
          ),
          shiny::tabPanel(
            icon = shiny::icon("table"),
            title = "Tableau",
            mod_zmpap_indicator_ui(ns("zmpap_indicator_1"))
          ),
          shiny::tabPanel(
            icon = shiny::icon("chart-bar"),
            title = "Diagramme",
            mod_zmpap_graph_ui(ns("zmpap_graph_1"))
          ),
          shiny::tabPanel(
            icon = shiny::icon("location-dot"),
            title = "Carte",
            mod_zmpap_map_ui(ns("zmpap_map_1"))
        )
      )
    )
}

#' zmpap Server Functions
#'
#' @noRd
mod_zmpap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_zmpap_welcome_server("zmpap_welcome_1")
    mod_zmpap_indicator_server("zmpap_indicator_1")
    mod_zmpap_graph_server("zmpap_graph_1")
    mod_zmpap_map_server("zmpap_map_1")
  })
}

## To be copied in the UI
# mod_zmpap_ui("zmpap_1")

## To be copied in the server
# mod_zmpap_server("zmpap_1")
