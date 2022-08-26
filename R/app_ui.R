#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
      shiny::navbarPage(
        title ="HTI - MSNA 2022 - Résultats préliminaires",
        # tags$style(
        #   type = "text/css",
        #   ".shiny-output-error { visibility: hidden; }",
        #   ".shiny-output-error:before { visibility: hidden; }"
        # ),
        mod_welcome_ui("welcome_1"),
        mod_indicator_main_ui("indicator_main_1"),
        mod_graph_main_ui("graph_main_1"),
        mod_map_main_ui("map_main_1"),
        mod_apropos_ui("apropos_1")
          # )
        )
    )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HTI.MSNA.2022"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
