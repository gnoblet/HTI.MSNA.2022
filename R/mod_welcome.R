#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel("Introduction",
           shiny::sidebarLayout(
             shiny::sidebarPanel(
               radioButtons("plotType", "Plot type",
                            c("Scatter"="p", "Line"="l")
               )
             ),
             mainPanel(
                shinydashboardPlus::box(plotOutput(ns("welcome_plot")))
             )
           )
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    x <- 1:5
    y <- 6:10

    output$welcome_plot <- renderPlot(
      plot(x,y)
    )
  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
