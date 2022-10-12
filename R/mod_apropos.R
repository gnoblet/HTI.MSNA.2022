#' a-propos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_apropos_ui <- function(id) {
  ns <- NS(id)

  shiny::tabPanel(
    "A propos",
    value = "panel-a-propos",
    icon = shiny::icon("circle-question"),
    shiny::fluidRow(
      class = "fluid-row-padding",
      shiny::column(
        6,
        shiny::fluidRow(
          shiny::wellPanel(
            shiny::h2("Téléchargement, informations complétementaires et contacts"),
            shiny::p(" Si vous souhaitez de plus amples informations sur les objectifs de la recherche et la méthodologie, les termes de référence de l'évaluation sont disponibles ", shiny::tags$a("ici.", class = "a", href = "https://www.impact-repository.org/document/reach/dc728cf9/REACH_HTI_tor_MSNA-2022-1.pdf"), "Le questionnaire a été mis au point avec les partenaires sectoriels et les groupes de travail thématiques au sein de la coordination humanitaire d'Haïti. Il est traduit en créole haïtien. Le questionnaire est disponible ", shiny::tags$a("ici.", class = "a", href = "https://www.impact-repository.org/document/reach/b2448f66/REACH_HTI_dap_MSNA-2022-1-1.xlsx"), "Si vous souhaitez des analyses complémentaires ou plus d'informations, vous pouvez contacter", shiny::tags$a("guillaume.noblet@reach-initiative.org", class = "a", href = "mailto:guillaume.noblet@reach-initiative.org")),
            shiny::p("Le jeu de données est disponible sur le", shiny::tags$a("REACH Resource Center", class = "a", href = "https://www.impact-repository.org/document/reach/60f94a18/REACH_HTI_dataset_without-West_MSNA-2022.xlsx"), ". L'intégralité des résultats des analyses présentées dans le tableau de bord sont accessibles sur demande.")
          )
        ),
        shiny::fluidRow(
          shiny::wellPanel(
            shiny::h2("Méhodologie"),
            shiny::p("Trois groupes de population ont été identifiés comme prioritaires dans le cadre de la MSNA – la population générale et les populations déplacées et rapatriées enregistrées auprès de l’OIM. Les trois groupes sont les suivants :"),
            reactable::reactableOutput(ns("table_group_pop")),
            shiny::br(),
            shiny::p("REACH Initiative a effectué la collecte pour les ménages en population générale. La collecte de données a eu lieu du 12 juin au 13 septembre. 3896 ménages ont participé à l'enquête, dont 1188 dans la Zone métropolitaine de Port-au-Prince. Les entretiens ont été effectués en personne. L'échantillon est stratifié par grappes avec un niveau de confiance de 95% et une marge d'erreur de 10%. Les données sont disponibles au niveau des départements et des milieux (soit rural, soit urbain), et au niveau des communes pour la Zone métropolitaine de Port-au-Prince. L'échantillon n'a pas pu être complété pour le département de l'Ouest en zone rurale du fait des contraintes sécuritaires de septembre 2022, les résultats sont donc à considérer comme indicatifs. Pour les populations déplacées et rapatriées, les données ont été collectées par l'OIM.")
          )
        )
      ),
      shiny::column(
        6,
        shiny::fluidRow(
          class = "fluid-row-padding",
          shiny::wellPanel(
            shiny::h2("Limites"),
            shiny::p("Quelques limites de l'évaluation :", tags$ul(
              tags$li(shiny::strong("Définition des milieux :"), "les milieux rural et urbain ont été définis à partir d'une analyse géospatiale des composantes infrastructurales des zones urbaines, elles ne correspondent pas à des ensembles socio-économiques."),
              tags$li(shiny::strong("Couverture géographique :"), "l’analyse des besoins humanitaires dans le cadre de la Revue des besoins humanitaires et du Plan de réponse humanitaire en Haïti se produit au niveau communal. Les données de la MSNA sont cependant disponibles au niveau des départements et des milieux."),
              tags$li(shiny::strong("Biais de réponse :"), "certains indicateurs comme la non-satisfaction du comportement des travailleurs humanitaires, l’enrôlement des enfants dans les groupes armées, les incidents graves de protection ou les violences basées sur le genre peuvent avoir été sous-rapportés en fonction de la sensibilité de la question et de la subjectivité et de la perception des personnes interrogées."),
              tags$li(shiny::strong("Sous-ensembles :"), "les résultats faisant référence à un sous-ensemble de la population totale peuvent avoir une marge d’erreur plus grande, ce qui peut induire un niveau de précision plus bas. Ainsi, les résultats relatifs à des sous-ensembles de la population sont indiqués comme tels chaque fois qu'ils sont déclarés."),
              tags$li(shiny::strong("Limites des entretiens avec le chef ou la cheffe de ménage :"), "comme le ménage est l’unité d’analyse, les dynamiques au sein des ménages (par exemple les relations de pouvoir au sein du ménage en fonction du genre, de l’âge ou du handicap) ne peuvent être appréhendées. REACH vous encourage à consulter d’autres sources de données afin de compléter résultats du MSNA."),
              tags$li(shiny::strong("Période de collecte de données :"), "pour l’interprétation des résultats, notamment de sécurité alimentaire en fonction des différentes zones agro-écologiques, la période de collecte de données pour chaque département est précisée sur la carte de couverture.")
            )),
            shiny::p("Les limites seront présentées de manière plus détaillée dans l'annexe méthodologique (à venir).")
          )
      ),
      shiny::fluidRow(
        class = "fluid-row-padding",
        shiny::wellPanel(
          shiny::h2("Acronymes"),
          tags$ul(
            tags$li(shiny::strong("ZMPAP :"), "zone métropolitaine de Port-au-Prince"),
            tags$li(shiny::strong("MSNA :"), "évaluation multisectorielle des besoins ou ", shiny::em("multi-sectoral needs assessment "), "en Anglais"),
            tags$li(shiny::strong("EPHA :"), "eau potable, hygiène et assainissement"),
            tags$li(shiny::strong("ABNA :"), "abris et biens non alimentaires"),
            tags$li(shiny::strong("OIM : "), "Organisation Internationale pour les Migrations"),
            tags$li(shiny::strong("GCIS : "), "Groupe de coordination inter-secteur")
            )
          )
          )
        )
    ),
    shiny::fluidRow(
      class = "fluid-row-padding",
      shiny::column(
        6,
        shiny::h2("La MSNA a été conduite au sein du cadre institutionnel de :"),
        shiny::fluidRow(
          class = "fluid-row-logo",
          shiny::column(
            6,
            shiny::img(src = "www/OCHA.png", height = "50px")
          ),
          shiny::column(
            6,
            shiny::img(src = "www/GCIS.png", height = "50px")
          )
        )
      ),
      shiny::column(
        6,
        shiny::fluidRow(
          class = "fluid-row-padding",
          shiny::h2("Financé par :"),
          shiny::column(
            6,
            shiny::img(src = "www/USAID.png", height = "50px")
          ),
          shiny::column(
            6,
            shiny::img(src = "www/ECHO.png", height = "50px")
          )
        ),
        shiny::fluidRow(
          class = "fluid-row-padding",
          shiny::h2(" "),
          shiny::column(
            6,
            shiny::img(src = "www/WFP.png", height = "50px")
            ),
          shiny::column(
            6,
            shiny::img(src = "www/UNICEF.png", height = "50px")
          )
        )
      )
    ),
    shiny::fluidRow(
      class = "fluid-row-padding",
      shiny::column(
        6,
        shiny::h2("Partenaire opérationnel principal :"),
        shiny::img(src = "www/ACTED.png", height = "50px")
      ),
      shiny::column(
        6,
        shiny::fluidRow(
        class = "fluid-row-padding",
        shiny::h2("Autres financements et partenariats :"),

          shiny::column(
            6,
            shiny::img(src = "www/WFP.png", height = "50px")
          ),

          shiny::column(
            6,
            shiny::img(src = "www/CRS.png", height = "50px")
          )
        ),
        shiny::fluidRow(
          class = "fluid-row-padding",
          shiny::h2(" "),

          shiny::column(
            6,
            shiny::img(src = "www/SAVE.png", height = "40px")
          ),
          shiny::column(
            6,
            shiny::img(src = "www/CONCERN.png", height = "50px")
          )
        ),
        shiny::fluidRow(
          class = "fluid-row-padding",
          shiny::h2(" "),
          shiny::column(
            6,
            shiny::img(src = "www/FLOWMINDER_blue.png", height = "32px")
          ),
          shiny::column(
            6,
            shiny::img(src = "www/WORLDVISION.png", height = "50px")
          )
        )
      )
    ),
    shiny::br(height = "20px")
  )
}

#' a-propos Server Functions
#'
#' @noRd
mod_apropos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table_group_pop <- reactable::renderReactable({
      dplyr::tibble(
        `Groupe de population` = c("Ménage en population générale", "Ménage déplacé", "Ménage rapatrié"),
        `Description` = c(
          "Le ménage n'est actuellement pas dans une situation de déplacement à la suite d'un choc et se trouve dans son lieu de résidence habituel.\n Le ménage est dans une situation de déplacement, mais n’est pas enregistré auprès de la Direction Générale de la Protection Civile (DGPC) et de l’OIM.",
          "Le ménage ne se trouve pas actuellement dans son lieu de résidence habituel qu'il a dû quitter en raison d'un choc et est enregistré comme ménage déplacé interne auprès de la DGPC et de l’OIM.",
          "Le ménage se trouve actuellement en Haïti après une migration hors du pays et est enregistré comme rapatrié auprès de la DGPC et de l’OIM."
        )
      ) |>
        as.data.frame() |>
        reactable::reactable(
          class = "reactable",
          rownames = FALSE,
          defaultPageSize = 50,
          bordered = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            borderColor = visualizeR::cols_reach("main_red"),
            stripedColor = visualizeR::cols_reach("red_alt_5"),
            color = visualizeR::cols_reach("black")
          ),
          columns = list(
            "Groupe de population" = reactable::colDef(maxWidth = 200)
          ),
          style = list(fontSize = "10px"),
          defaultColDef = reactable::colDef(
            style = list(
              color = visualizeR::cols_reach("black") #  ,
              # background = visualizeR::cols_reach("main_red")
            ),
            vAlign = "center",
            # maxWidth = 300,
            align = "left",
          )
        )
    })
  })
}

## To be copied in the UI
# mod_apropos_ui("apropos_1")

## To be copied in the server
# mod_apropos_server("apropos_1")
