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
                  value = "panel-welcome",
                  div(class="outer",

                  leaflet::leafletOutput(ns("map"), height = "100%"),

                  shiny::absolutePanel(
                    id = "presentation",
                    class = "well",
                    fixed = TRUE,
                    draggable = F,
                    top = 90,
                    left = 30,
                    right = "auto",
                    width = 400,
                    shiny::h2("Présentation du projet"),
                    shiny::p("La nature multiforme de la crise en Haïti explique que la coordination humanitaire estime à 4,9 millions le nombre de personnes dans le besoin en 2022. Tandis que l’accès humanitaire représente un obstacle de plus en plus tangible à la collecte d’information, en raison du caractère enclavé de certaines zones et du contexte sécuritaire volatile, les données disponibles sont généralement spécifiques à une intervention, un lieu ou un secteur."),
                    shiny::p("Afin de répondre à ces défis en termes de gestion de l’information, REACH, sous le mandat du GCIS a facilité pour la première fois en Haïti une Evaluation multisectorielle des besoins (MSNA) qui couvre l'ensemble du territoire afin d'informer le Cycle de programmation humanitaire (HPC) 2023."),
                    shiny::p("Le questionnaire a été mis au point avec les partenaires sectoriels et les groupes de travail thématiques. Il est traduit en créole haïtien."),
                    shiny::br(),
                    shiny::img(src = "www/reach_logo.png", width = "60%", align = "left")
                  ),
                  shiny::absolutePanel(
                    id = "method",
                    class = "well",
                    fixed = TRUE,
                    draggable = F,
                    top = 90,
                    left = "auto",
                    right = 30,
                    width = 400,
                    shiny::h2("Méhodologie"),
                    shiny::p("La collecte de données pour l'ensemble des départements à l'exception de l'Ouest a eu lieu du 13 juin à 10 août. 2484 ménages ont participé à l'enquête. Les entretiens ont été effectués en personne. "),
                    shiny::p("La collecte de données pour l'Ouest et la Zone métropolitaine de Port-au-Prince est encore en cours."),
                    shiny::p("REACH a effectué la collecte pour les ménages en population générale. L'échantillon est stratifié par grappes avec un niveau de confiance de 95% et une marge d'erreur de 10%. Les données sont disponibles au niveau des départements et des milieux (soit rural, soit urbain). Voir carte de couverture ci-contre."),
                   shiny::tags$ul(
                      shiny::tags$li("Le questionnaire est disponible :", shiny::tags$a("ici.", href = "https://www.impact-repository.org/document/reach/b2448f66/REACH_HTI_dap_MSNA-2022-1-1.xlsx")),
                      shiny::tags$li("Pour plus d'informations sur la méthodologie, voir l'onglet", shiny::tags$em("A propos.")))
                    )
                  # ,
                  # shiny::absolutePanel(
                  #   id = "reach-logo",
                  #   #class = "well",
                  #   fixed = TRUE,
                  #   draggable = F,
                  #   top = 1000,
                  #   left = 30,
                  #   right = "auto",
                  #   width = 350,
                  #   shiny::img(src = "www/reach_logo.png", width = "80%", align = "right")
                  # )
                )
  )

}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #------ Colors
    white <- visualizeR::cols_reach("white")
    red_pal <- visualizeR::pal_reach("red_alt", reverse = TRUE)
    main_red <- visualizeR::cols_reach("main_red")
    main_grey <- visualizeR::cols_reach("main_grey")
    main_lt_grey <- visualizeR::cols_reach("main_lt_grey")


    admin0_frontier <- HTI.MSNA.2022::hti_admin0_frontier
    admin0_border <- HTI.MSNA.2022::hti_admin0_border

    #----- Spatial data
    admin1_polygon <- HTI.MSNA.2022::hti_admin1_polygon |>
      dplyr::mutate(admin1 = stringr::str_replace_all(stringr::str_to_lower(departemen), " |-|'", "_")) |>
      dplyr::mutate(admin1 = ifelse(admin1 == "grande_anse", "grand_anse", admin1))

    admin1_line <- HTI.MSNA.2022::hti_admin1_line

    admin1_centroid <- admin1_line |>
      sf::st_point_on_surface()

    admin1_labels_halo <- sprintf('<strong><span style="font-size: 18px; color: %s">%s</span></strong>',
                                  main_grey, admin1_centroid$ADM1_FR
    ) |>
      lapply(htmltools::HTML)

    #------ Spatial stratum
    stratum <- HTI.MSNA.2022::hti_stratum |>
      janitor::clean_names() |>
      dplyr::filter(!(strate %in% c("ouest_urbain", "ouest_rural")))

    #------ Basic info
    basic_info <- HTI.MSNA.2022::hti2202_basic_info |>
      dplyr::rename(milieu_id = milieu)

    stratum <- dplyr::left_join(
      stratum,
      basic_info,
      by = c("strate" = "stratum")
    )

    fillcol <- leaflet_color_factor(pal = c("#877c60", main_lt_grey) , domain = stratum$milieu)
    fillcol_legend <- leaflet_color_factor(pal = c("#877c60", main_lt_grey), domain = stratum$milieu)


    #------ Map


    choice_map <- shiny::reactive({

      label_stratum <- sprintf(
        "<div class ='leaflet-hover'>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 18px; color: %s; font-weight: bold;'> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Nombre d'entretiens : </strong> %s </span><br>
        <span style = 'font-size: 14px; color: %s;'> <strong>  Dates de collecte : </strong>  %s - %s </span><br>
        </div>
        ",
        main_grey,
        stratum$department,
        main_red,
        stratum$milieu,
        main_grey,
        stratum$n,
        main_grey,
        stratum$start_name,
        stratum$end_name
      ) |>
        lapply(htmltools::HTML)


      leaflet::leaflet(
        stratum,

        #------ Options
        options = leaflet::leafletOptions(
          zoomControl = FALSE,
          doubleClickZoom = FALSE,
          zoomSnap = 0.01,
          zoomDelta = 0.01,
          attributionControl = FALSE,
          dragging = FALSE,
          scrollWheelZoom = FALSE,
          easeLinearity = 0.35,
          minZoom = 8,
          maxZoom = 9)) |>

        #------ Providers
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels,
                                  options = leaflet::providerTileOptions(opacity = 0.5)) |>

        #------ Polygons
        leaflet::addPolygons(
          fillColor = ~fillcol(milieu),
          weight       = 0.2,
          smoothFactor = 0.5,
          opacity      = 0.9,
          fillOpacity  = 0.9,
          options      = list(zIndex = 400),
          label = label_stratum,

          #------ Highlight
          highlightOptions = leaflet::highlightOptions(fillColor = main_red,
                                                       color        = main_red,
                                                       weight       = 2,
                                                       opacity      = 0.9,
                                                       fillOpacity  = 0.5,
                                                       bringToFront = F),
          labelOptions = leaflet::labelOptions(
            noHide = FALSE,
            noWrap = FALSE,
            opacity = 0.9,
            direction = "auto",
            offset = c(-10,0),
            textOnly = F,
            style = list(
              "padding" = "3px 8px",
              "font-family" = "Leelawadee UI, Leelawadee",
              "border-color" = "#EE5859"
            )
          )

          ) |>

        #------ Limites administratives
        leaflet::addPolylines(
          data = admin1_line,
          color = main_grey,
          weight = 1.3,
          opacity = 1.0,
          options = list(zIndex = 400)
        ) |>

        #------- Limites administratives : contour
        leaflet::addPolylines(
          data = admin0_border,
          color = "#000000",
          fillOpacity = 0,
          weight = 1.5,
          opacity = 1.0,
          options = list(zIndex = 400)
        ) |>

        #------- Limites administratives : frontière
        leaflet::addPolylines(
          data = admin0_frontier,
          color = "#000000",
          weight = 2,
          opacity = 1.0,
          options = list(zIndex = 400)
        ) |>

        #------ Label Admin 1 :
        leaflet::addLabelOnlyMarkers(
          data = admin1_centroid,
          label = admin1_labels_halo,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "padding"     = "3px 8px",
              "font-family" = "Leelawadee UI",
              "text-shadow" = sprintf("-0.8px 0 %s, 0 0.8px %s, 0.8px 0 %s, 0 -0.8px %s, 0 0 7px %s", white, white, white, white, white)
            )
          )
        ) |>
        leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE)) |>

        #------ Legend
        leaflet::addLegend(
          position = "bottomright",
          opacity = 1,
          pal = fillcol_legend,
          values = ~milieu,
          na.label = "Données manquantes",
          title = "Milieu"
        ) |>
        leaflet::addScaleBar(position = "bottomleft", leaflet::scaleBarOptions(imperial = FALSE))

  })

  output$map <- leaflet::renderLeaflet({
    choice_map()
  })

  }
)}
## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
