#' graph_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_main_ui <- function(id){
  ns <- NS(id)

  shiny::tabPanel(
    "Graphes",
    shiny::sidebarPanel(
      width = 3,
        shinyWidgets::prettyRadioButtons(
          inputId = ns("disagg"),
          label = "Niveau géographique",
          choices = c("National", "Départemental"),
          selected = "National",
          fill = TRUE,
          status = "danger"),
        shiny::selectInput(
          inputId = ns("rq"),
          label = "Secteur",
          choices = c("Information générale", "Démographie du ménage", "Déplacement", "Washington Group", "Santé", "Education", "Sécurité alimentaire", "Moyens de subsistance", "ABNA", "EPHA", "Protection", "Redevabilité"),
          selected = "EPHA"),
        shiny::selectInput(
          inputId = ns("sub_rq"),
          label = "Sous-secteur",
          choices = "Accès à l'eau",
          selected = "Accès à l'eau"),
        shiny::selectInput(
          inputId = ns("indicator"),
          label = "Indicateur",
          choices = "% de ménages par source d'eau de boisson",
          selected = "% de ménages par source d'eau de boisson"),
      shiny::conditionalPanel(
        condition = "input.disagg == 'Départemental'",
        ns = ns,
          shiny::selectInput(
            inputId = ns("choice"),
            label = "Choix de réponse",
            choices = "Source protégée",
            selected = "Source protégée")
      ),
        shiny::img(src = "www/reach_logo.png", width = "80%", align = "center")
    ),
    shiny::mainPanel(
      shiny::h3(shiny::textOutput(ns("indicator_name"))),
      shiny::conditionalPanel(
        condition = "input.disagg == 'Départemental'",
        ns = ns,
        shiny::h3("Choix : ", shiny::textOutput(ns("choice_name")))
        ),
      shiny::plotOutput(ns("graph"))
    )

  )
}

#' graph_main Server Functions
#'
#' @noRd
mod_graph_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    analysis <- reactive({
      switch(input$disagg,
             "National" = HTI.MSNA.2022::data_main_simple,
             "Départemental" = HTI.MSNA.2022::data_admin1_simple)
    })


    shiny::observeEvent(input$rq, {
      shiny::updateSelectInput(session,
                               "sub_rq",
                               choices = analysis() |>
                                 dplyr::filter(
                                   rq == input$rq
                                 ) |>
                                 dplyr::pull(sub_rq) |>
                                 unique()
      )
    })

    shiny::observeEvent(input$sub_rq, {
      shiny::updateSelectInput(session,
                               "indicator",
                               choices = analysis() |>
                                 dplyr::filter(
                                   rq == input$rq,
                                   sub_rq == input$sub_rq
                                 ) |>
                                 dplyr::pull(indicator) |>
                                 unique()
      )
    })

    shiny::observeEvent(input$indicator, {
      shiny::updateSelectInput(session,
                               "choice",
                               choices = analysis() |>
                                 dplyr::filter(
                                   rq == input$rq,
                                   sub_rq == input$sub_rq,
                                   indicator == input$indicator
                                 ) |>
                                 dplyr::pull(choices_label) |>
                                 unique()
      )
    })




    output$indicator_name <- shiny::renderText({
      analysis_filtered <- analysis() |>
        dplyr::filter(indicator == input$indicator) |>
        dplyr::pull(indicator) |>
        unique()
    })


  output$choice_name <- shiny::renderText({
    if(!(input$disagg %in% c("National"))) {
        analysis_filtered <- analysis() |>
          dplyr::filter(indicator == input$indicator, choices_label == input$choice) |>
          dplyr::pull(choices_label) |>
          unique()
    } else {
      "Aucun choix sélectionné"
    }
    })



    output$graph <-
      shiny::renderPlot(
        height = 700,
        expr = {
        analysis_filtered <- analysis() |>
          dplyr::filter(rq == input$rq,
                        sub_rq == input$sub_rq,
                        indicator == input$indicator,
                        ) |>
          dplyr::mutate(stat = ifelse(is.na(stat), 0, stat)) |>
          dplyr::arrange(dplyr::desc(stat)) |>
          dplyr::mutate(stat = ifelse(analysis_name == "Proportion" & analysis != "ratio", round(stat * 100, 0), round(stat, 1))) |>
          dplyr::mutate(choices_label = dplyr::case_when(
            analysis_name == "Moyenne" ~ "Moyenne",
            analysis_name == "Médiane" ~ "Médiane",
            TRUE ~ choices_label)
          )


       if (input$disagg == "National") {

         if (nrow(analysis_filtered) == 0) {
           graph <- visualizeR::hbar(
             .tbl = tibble::tibble(stat = 100, choices_label = "Missing data"),
             x = stat,
             y = choices_label,
             font_size = 16,
             font_family = "Leelawadee UI",
             reverse = TRUE,
             theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 14))
         } else {

           graph <- visualizeR::hbar(
             .tbl = analysis_filtered |>
               dplyr::mutate(choices_label = factor(choices_label, levels = unique(analysis_filtered$choices_label))),
             x = stat,
             y = choices_label,
             font_family = "Leelawadee UI",
             font_size = 16,
             initiative = "reach",
             pal = "primary",
             reverse = TRUE,
             theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 14))
         }

        } else if (input$disagg == "Départemental") {

          analysis_filtered_dept <- analysis_filtered |>
            dplyr::filter(choices_label == input$choice)

          if (nrow(analysis_filtered_dept) == 0) {
            graph <- visualizeR::hbar(
              .tbl = tibble::tibble(stat = 100, choices_label = "Missing data"),
              x = stat,
              y = choices_label,
              font_size = 16,
              font_family = "Leelawadee UI",
              reverse = TRUE,
              theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 14))
          } else {

            graph <- visualizeR::hbar(
              .tbl = analysis_filtered_dept |>
                dplyr::mutate(group_disagg_label = factor(group_disagg_label, levels = unique(analysis_filtered_dept$group_disagg_label))),
                x = stat,
                y = group_disagg_label,
                font_family = "Leelawadee UI",
                font_size = 16,
                initiative = "reach",
                pal = "primary",
                reverse = TRUE,
                theme = ggblanket::gg_theme(font = "Leelawadee UI", size_body = 14))
          }
        }

        return(graph)
      })

  })
}

## To be copied in the UI
# mod_graph_main_ui("graph_main_1")

## To be copied in the server
# mod_graph_main_server("graph_main_1")
