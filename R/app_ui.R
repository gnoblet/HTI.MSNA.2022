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
        "HTI - MSNA 2022 - Résultats préliminaires",
        mod_welcome_ui("welcome_1"),
        # shiny::navbarMenu(
        #   "Indicateurs",
        #   menuName = "indicators",
          mod_indicator_main_ui("indicator_main_1"),
          mod_indicator_admin1_ui("indicator_admin1_1")
          # )
        )
    )
               # ,
               # theme =  bslib::bs_theme(
               #   bootswatch = "cerulean",
               #   bg = "#FFFFFF",
               #   fg = "#58585A",
               #   primary = "#EE5859",
               #   secondary = "#FFFFFF",
               #   success = "#EE5859",
               #   warning = "#EE5859",
               #   danger = "#EE5859")


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
