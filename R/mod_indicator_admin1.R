#' indicator_admin1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_admin1_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    "Par département",
    shiny::sidebarPanel(
      width = 3,
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("main_rq"),
          label = "Secteur",
          choices = unique(HTI.MSNA.2022::data_admin1_simple$rq),
          selected = "EPHA")
      ),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("main_sub_rq"),
          label = "Sous-secteur",
          choices = unique(HTI.MSNA.2022::data_admin1_simple$sub_rq),
          selected = "Accès à l'eau")
      ),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("main_indicator"),
          label = "Indicateur",
          choices = unique(HTI.MSNA.2022::data_admin1_simple$indicator),
          selected = "% de ménages par source d'eau de boisson")
      ),
      shiny::fluidRow(
        shiny::downloadButton(
          ns("main_download"),
          label = "Télécharger la table"
        )
      ),
      shiny::fluidRow(
        shiny::img(src = "www/reach_logo.png", width = "80%", align = "center")
      )
    ),
    shiny::mainPanel(
      shiny::h3(shiny::textOutput(ns("main_indicator_name"))),
      reactable::reactableOutput(ns("main_table"))
    )
  )
}

#' indicator_admin1 Server Functions
#'
#' @noRd
mod_indicator_admin1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    analysis_admin1_simple <- HTI.MSNA.2022::data_admin1_simple |>
      dplyr::mutate(stat = ifelse(analysis_name == "Proportion" & analysis != "ratio", round(stat * 100, 0), round(stat, 1)))

    shiny::observeEvent(input$main_rq, {
      shiny::updateSelectInput(session,
                               "main_sub_rq",
                               choices = analysis_admin1_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq
                                 ) |>
                                 dplyr::pull(sub_rq) |>
                                 unique()
      )
    })

    shiny::observeEvent(input$main_sub_rq, {
      shiny::updateSelectInput(session,
                               "main_indicator",
                               choices = analysis_admin1_simple |>
                                 dplyr::filter(
                                   rq == input$main_rq,
                                   sub_rq == input$main_sub_rq
                                 ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })



    output$main_indicator_name <- shiny::renderText({
      analysis_filtered <- analysis_admin1_simple |>
        dplyr::filter(indicator == input$main_indicator) |>
        dplyr::pull(indicator) |>
        unique()
    }
    )

    analysis_filtered <- shiny::reactive({
      analysis_admin1_simple |>
        dplyr::filter(rq == input$main_rq,
                      sub_rq == input$main_sub_rq,
                      indicator == input$main_indicator) |>
        tidyr::pivot_wider(
          id_cols = c(id_analysis, indicator, choices, recall, subset, choices_label),
          names_from = group_disagg_label,
          values_from = stat) |>
        impactR::deselect(id_analysis, choices, indicator) |>
        dplyr::arrange(dplyr::desc(dplyr::across(where(is.numeric)))) |>
        dplyr::rename("Option de réponse" = choices_label, "Sous-ensemble" = subset, "Période de rappel" = recall) |>
        janitor::remove_empty(which = "cols") |>
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x)))
    })


    output$main_download <- shiny::downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        writexl::write_xlsx(data, file)
      }
    )

    output$main_table <- reactable::renderReactable({
      analysis_filtered <- analysis_admin1_simple |>
        dplyr::filter(rq == input$main_rq,
                      sub_rq == input$main_sub_rq,
                      indicator == input$main_indicator) |>
        dplyr::arrange(dplyr::desc(stat)) |>
        tidyr::pivot_wider(
          id_cols = c(id_analysis, indicator, choices, recall, subset, choices_label),
          names_from = group_disagg_label,
          values_from = stat) |>
        impactR::deselect(id_analysis, choices, indicator) |>
        dplyr::rename("Option de réponse" = choices_label, "Sous-ensemble" = subset, "Période de rappel" = recall) |>
        janitor::remove_empty(which = "cols") |>
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))) |>
        as.data.frame()

      reactable::reactable(
        analysis_filtered,
        rownames = FALSE,
        bordered = TRUE,
        # striped = TRUE,
        highlight = TRUE,
        # compact = TRUE,
        filterable = TRUE,
        height = "800px",
        style = list(fontFamily = "Leelawadee UI"),
        defaultColDef = colDef(
          cell = reactablefmtr::color_tiles(
            analysis_filtered,
            colors = c("white","#EE5859"),
            box_shadow = TRUE,
            span = TRUE),
          align = "center"))
    })

  })
}

## To be copied in the UI
# mod_indicator_admin1_ui("indicator_admin1_1")

## To be copied in the server
# mod_indicator_admin1_server("indicator_admin1_1")
