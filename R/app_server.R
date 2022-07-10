#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_welcome_server("welcome_1")
  mod_indicator_main_server("indicator_main_1")
  mod_indicator_admin1_server("indicator_admin1_1")
  mod_graph_main_server("graph_main_1")
  mod_graph_admin1_server("graph_admin1_1")
}
