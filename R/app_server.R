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
  mod_graph_main_server("graph_main_1")
  mod_map_main_server("map_main_1")
  mod_apropos_server("apropos_1")
}
