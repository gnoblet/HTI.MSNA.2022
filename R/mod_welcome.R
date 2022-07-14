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
                  icon = shiny::icon("flag"),
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 6,
                      title = "Présentation du projet",
                      shiny::tagList(
                        shiny::p("La nature multiforme de la crise en Haïti explique que la coordination humanitaire estime à 4,9 millions le nombre de personnes dans le besoin en 2022.  Sollicitée dans le cadre de réponses d’urgence face à des aléas climatiques extrêmes, la coordination humanitaire est confrontée à des défis pour parvenir à une compréhension holistique de la crise et des vulnérabilités chroniques qui affectent la population. Tandis que l’accès humanitaire représente un obstacle de plus en plus tangible à la collecte d’information, en raison du caractère enclavé de certaines zones et du contexte sécuritaire volatile, les données disponibles sont généralement spécifiques à une intervention, un lieu ou un secteur. Bien qu’elles soient essentielles pour étayer la programmation des acteurs humanitaires, ces données ne permettent pas à la coordination humanitaire de revoir leur stratégie de réponse à l’échelle de toute la crise et d’assurer que les populations les plus vulnérables accèdent à l’aide humanitaire dont elles ont besoin."),
                        shiny::p("Afin de répondre à ces défis en termes de gestion de l’information, REACH, sous le mandat du GCIS et avec l’approbation de l’Equipe Humanitaire Pays (EHP), se propose de faciliter pour la première fois en Haïti une Evaluation multisectorielle des besoins (MSNA) qui couvrirait l’ensemble du territoire sur les mois de juin et de juillet 2022. Cette évaluation permettra de mettre à disposition de la communauté humanitaire des informations comparables entre zones et entre groupes de population, participant ainsi à un renforcement du potentiel analytique basé sur l’évidence et favorisant une prise de décision et une planification humanitaire plus informée sur les besoins des ménages haïtiens en 2022. Coordonnée à travers le GTGI, cette évaluation a pour objectif d’informer le Cycle de programmation humanitaire (HPC) pour 2023.")
                        )),
                    shinydashboard::box(
                      width = 6,
                      title = "Méhodologie",
                      shiny::tagList(
                        shiny::p("L’outil d’enquête est mis au point à partir de consultations avec les partenaires sectoriels (Education, Santé, Protection, Protection de l’enfance, Sécurité alimentaire, ABNA, EPHA) et les groupes de travail thématiques (Groupe de travail Déplacés, Groupe de travail Redevabilité, Groupe de travail Transferts monétaires, GENCAP). Ces consultations permettront d’identifier des indicateurs clés sur les besoins sectoriels, l’accès et les préférences en termes d’assistance, les vulnérabilités et les chocs, à renseigner lors de l’enquête. La collecte de données aura lieu au cours des mois de juin et juillet 2022."),
                        shiny::p("Les différents chocs n’impactent pas l’ensemble des populations haïtiennes de manière uniforme sur le territoire. Il apparaît dès lors important d’obtenir des données fiables et impartiales pour chacun des dix départements, afin d’assurer que les enjeux particuliers qui affectent chaque zone soient reflétés dans les résultats. Trois groupes de population ont été identifiés comme prioritaires dans le cadre de l’évaluation – les populations non déplacées, les populations déplacées et les populations rapatriés.")
                      )
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

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
