#' a-propos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_apropos_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    "A propos",
    value = "panel_a-propos",
    icon = shiny::icon("circle-question"),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::wellPanel(
                      shiny::h2("Méhodologie"),
                      shiny::p("La collecte de données a eu lieu de 14 juin à 15 août et 2484 ménages ont participé à l'enquête. Les entretiens ont été effectués en personne"),
                      shiny::p("REACH a effectué la collecte pour les ménages en population générale. L'échantillon était un échantillon stratifié par grappes avec un niveau de confiance de 95% et une marge d'erreur de 10%. Les données sont disponibles au niveau des départements et des milieux (soit rural soit urbain). Voir carte ci-contre."))
      ),
      shiny::column(6,
                    shiny::wellPanel(
                      shiny::p(shiny::tags$a("Disponible ici la méthodo", href = "https://www.impact-repository.org/document/reach/dc728cf9/REACH_HTI_tor_MSNA-2022-1.pdf"))
                    ))
    ),
    shiny::fluidRow(

    )
  )

}

#' a-propos Server Functions
#'
#' @noRd
mod_apropos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_apropos_ui("apropos_1")

## To be copied in the server
# mod_apropos_server("apropos_1")
