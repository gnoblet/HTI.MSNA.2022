#' outputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_outputs_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    title = "Publications",
    value = "panel-outputs",
    icon = shiny::icon("file"),
    shiny::fluidRow(
      shiny::sidebarPanel(
        shiny::h2("Où trouver les publications ?"),
        shiny::p("L'ensemble des publications de l'Evaluation multisectorielles des besoins (MSNA) 2022 peuvent être trouvées sur le ", shiny::tags$a("REACH Resource Center.", class = "a", href = "https://www.reachresourcecentre.info/country/haiti/cycle/48772/")),
        shiny::p("Le tableau de bord en Créole haïtien est accessible ", shiny::tags$a("ici.", class = "a", href = "https://reach-info.org/hti/msna2022/ht/"))
      ),
      shiny::mainPanel(
        shiny::h1("Liste de publications"),
        shiny::h2("Fiches d'information"),
        shiny::p(
          tags$ul(
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Bulletin MSNA - Analyse multisectorielle", class = "a", href = "https://www.impact-repository.org/document/reach/de634485/REACH_HTI_bulletin_MSNA-2022.pdf")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Fiche d'information - Résultats clés - Redevabilité", class = "a", href = "https://www.impact-repository.org/document/reach/a6aa31f9/REACH_HTI_factsheet_aap_brief_MSNA_2022.pdf")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Fiche d'information - Résultats clés - EPHA, Abris et santé", class = "a", href = "https://www.impact-repository.org/document/reach/fe9484b2/REACH_HTI_factsheet_Abris_EPHA_sante_MSNA_2022.pdf")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Fiche d'information - Résultats clés - Département du Nord", class = "a", href = "https://www.impact-repository.org/document/reach/aaa807b6/REACH_HTI_Factsheet_Nord_MSNA_2022.pdf"))
            )
        ),
        shiny::h2("Tableau de bord"),
        shiny::p(
          tags$ul(
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Tableau de bord interactif (fr)", class = "a", href = "https://reach-info.org/hti/msna2022/fr/")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Tableau de bord interactif (ht)", class = "a", href = "https://reach-info.org/hti/msna2022/ht/"))
          )
        ),
        shiny::h2("Annexe méthodologique, termes de référence et questionnaire"),
        shiny::p(
          tags$ul(
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Annexe méthodologique", class = "a", href = "https://www.impact-repository.org/document/reach/ba3c68fc/REACH_HTI_methodological-overview_MSNA-2022.pdf")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Plan d'analyse des données", class = "a", href = "https://www.impact-repository.org/document/reach/b2448f66/REACH_HTI_dap_MSNA-2022-1-1.xlsx")),
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Termes de référence", class = "a", href = "https://www.impact-repository.org/document/reach/dc728cf9/REACH_HTI_tor_MSNA-2022-1.pdf"))
          )
        ),
        shiny::h2("Données"),
        shiny::p(
          tags$ul(
            tags$li(shiny::tags$a("Evaluation multisectorielle des besoins (MSNA) 2022 - Données - Octobre 2022", class = "a", href = "https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx"))
          )
        )
      )
    )
  )
}

#' outputs Server Functions
#'
#' @noRd
mod_outputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_outputs_ui("outputs_1")

## To be copied in the server
# mod_outputs_server("outputs_1")
