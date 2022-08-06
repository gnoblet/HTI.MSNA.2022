#' @noRd
`%inT%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}

#' @noRd
toggleDisplayServer <- function (session, id, display = c("none", "block", "inline-block",
                                                          "table-cell"))
{
  display <- match.arg(display)
  session$sendCustomMessage(type = "toggleDisplay", message = list(id = id,
                                                                   display = display))
}


#' @noRd
my_selectize_group_server <- function (input, output, session, data, vars)
{
  ns <- session$ns
  toggleDisplayServer(session = session, id = ns("reset_all"),
                      display = "none")
  rv <- shiny::reactiveValues(data = NULL, vars = NULL)
  observe({
    if (is.reactive(data)) {
      rv$data <- data()
    }
    else {
      rv$data <- as.data.frame(data)
    }
    if (is.reactive(vars)) {
      rv$vars <- vars()
    }
    else {
      rv$vars <- vars
    }
    for (var in names(rv$data)) {
      if (var %in% rv$vars) {
        toggleDisplayServer(session = session, id = ns(paste0("container-",
                                                              var)), display = "table-cell")
      }
      else {
        toggleDisplayServer(session = session, id = ns(paste0("container-",
                                                              var)), display = "none")
      }
    }
  })
  # reactive({lapply(X = rv$vars, FUN = function(x) {
  #   vals <- sort(unique(rv$data[[x]]))
  #   selectizeInput(session = session, inputId = x, selected = vals$x,
  #                        choices = vals, server = TRUE)
  # })
  # })
  observe({
    lapply(X = rv$vars, FUN = function(x) {
      vals <- sort(unique(rv$data[[x]]))
      updateSelectizeInput(session = session, inputId = x,
                           choices = vals, server = TRUE)
    })
  })
  observeEvent(input$reset_all, {
    lapply(X = rv$vars, FUN = function(x) {
      vals <- sort(unique(rv$data[[x]]))
      updateSelectizeInput(session = session, inputId = x,
                           choices = vals, server = TRUE)
    })
  })
  observe({
    vars <- rv$vars
    lapply(X = vars, FUN = function(x) {
      ovars <- vars[vars != x]
      observeEvent(input[[x]], {
        data <- rv$data
        indicator <- lapply(X = vars, FUN = function(x) {
          data[[x]] %inT% input[[x]]
        })
        indicator <- Reduce(f = `&`, x = indicator)
        data <- data[indicator, ]
        if (all(indicator)) {
          toggleDisplayServer(session = session, id = ns("reset_all"),
                              display = "none")
        }
        else {
          toggleDisplayServer(session = session, id = ns("reset_all"),
                              display = "block")
        }
        for (i in ovars) {
          if (is.null(input[[i]])) {
            updateSelectizeInput(session = session,
                                 inputId = i, choices = sort(unique(data[[i]])),
                                 server = TRUE)
          }
        }
        if (is.null(input[[x]])) {
          updateSelectizeInput(session = session, inputId = x,
                               choices = sort(unique(data[[x]])), server = TRUE)
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    })
  })
  observe({
    updateSelectInput(inputId = "rq", choices = unique(rv$data$rq), selected = "EPHA")
  })
  observe({
    updateSelectInput(inputId = "id_analysis", choices = unique(rv$data$id_analysis), selected = "epha_1")
  })
  observe({
    updateSelectInput(inputId = "indicator", choices = unique(rv$data$indicator))
  })


  return(reactive({
    data <- rv$data
    vars <- rv$vars
    indicator <- lapply(X = vars, FUN = function(x) {
      data[[x]] %inT% input[[x]]
    })
    indicator <- Reduce(f = `&`, x = indicator)
    data <- data[indicator, ]
    return(data)
  }))
}


#' @noRd
label_format <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 1,
                          big.mark = ",", transform = identity)
{
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      if(max(cuts) <=5) {
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
               suffix)
      } else {
        paste0(prefix, formatNum(cuts[-n] + 1), between, formatNum(cuts[-1]),
               suffix)
      }
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n] + 1), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}


#'@noRd
leaflet_color_bin <- function(pal = visualizeR::pal_reach("red_alt"), domain, bins = 4, na_color = visualizeR::cols_reach("white"), right = TRUE, reverse = FALSE){
  leaflet::colorBin(
    pal,
    domain = domain,
    bins = bins,
    na.color = na_color,
    right = TRUE
  )
}


#' @noRd
info_box <- function(color_main_title = visualizeR::cols_reach("main_red"),
                     color_sub_title = visualizeR::cols_reach("main_red"),
                     color_pop_group = visualizeR::cols_reach("white"),
                     color_indicator = visualizeR::cols_reach("white"),
                     color_recall = visualizeR::cols_reach("white"),
                     color_subset = visualizeR::cols_reach("white"),
                     main_title = NULL,
                     sub_title = NULL,
                     pop_group = NULL,
                     indicator = NULL,
                     recall = NULL,
                     subset = NULL,
                     font_size_main_title = "22px",
                     font_size_sub_title = "18px",
                     font_size_pop_group = "14px",
                     font_size_indicator = "12px",
                     font_size_recall = "12px",
                     font_size_subset = "12px",
                     prefix_main_title = "",
                     prefix_sub_title = "",
                     prefix_pop_group = "",
                     prefix_indicator = "",
                     prefix_recall = "Période de rappel :",
                     prefix_subset = "Sous-ensemble :") {


  glue_string <- glue::glue(
                  "<span style = 'font-size: {font_size_main_title}; color: {color_main_title}; font-weight: bold; line-height: 1.2;'> <strong> {prefix_main_title} </strong> {main_title} </span>
                  <br>
                  <span style = 'font-size: {font_size_sub_title}; color: {color_sub_title}; font-weight: bold;line-height: 1.2;'> <strong> {prefix_sub_title} </strong> {sub_title} </span>
                  <br>
                  <span style = 'font-size: {font_size_pop_group}; color: {color_pop_group}; font-weight: bold;line-height: 1.2;'> <strong> {prefix_pop_group} </strong> {pop_group} </span>
                  <hr>
                  <span style = 'font-size: {font_size_indicator}; color: {color_indicator}; font-weight: bold;'> <strong> {prefix_indicator} </strong> {indicator} </span>
                  <hr>
                  <span style = 'font-size: {font_size_recall}; color: {color_recall};'> <strong> {prefix_recall} </strong> {recall} </span>
                  <br>
                  <span style = 'font-size: {font_size_subset}; color: {color_subset};'> <strong> {prefix_subset} </strong> {subset} </span>
                  ")

  html_output <-  glue_string |>
    shiny::HTML()

}


#' @noRd
ggplot_to_plotly <- function(ggplot, filename){
  plotly_plot <- plotly::ggplotly(ggplot) |>
    plotly::layout(
      xaxis = list(autorange = TRUE, fixedrange = TRUE)#,
      # yaxis = list(autorange = TRUE, fixedrange = TRUE)
    ) |>
    plotly::style(
      hoverinfo = "none"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      toImageButtonOptions = list(
        title = "Télécharger le graphique",
        format = "svg",
        # icon = shiny::icon("download"),
        filename = filename),
      modeBarButtonsToRemove = list("hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d")
      # modeBarButtonsToAdd = list(download_button)
    )

  return(plotly_plot)
}

#' @noRd
if_not_in_stop <- function(.tbl, cols, df, arg = NULL){
  if (is.null(arg)) {
    msg <- glue::glue("The following column/s is/are missing in `{df}`:")
  }
  else {
    msg <- glue::glue("The following column/s from `{arg}` is/are missing in `{df}`:")
  }
  if (!all(cols %in% colnames(.tbl))) {
    rlang::abort(
      c("Missing columns",
        "*" =
          paste(
            msg,
            paste(
              subvec_not_in(cols, colnames(.tbl)),
              collapse = ", ")
          )
      )
    )
  }
}

#' @noRd
abort_bad_argument <- function(arg1, must, not = NULL, arg2 = NULL, same = NULL) {
  msg <- glue::glue("`{arg1}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  if (!is.null(same) & !is.null(arg2)) {
    same <- typeof(same)
    msg_i <- glue::glue("`{arg2}` is {same}.")
  }

  rlang::abort("error_bad_argument",
               message = c(msg, "i" = msg_i),
               arg1 = arg1,
               must = must,
               not = not,
               arg2 = arg2,
               same = same
  )
}

#' @noRd
mutate_if_nulla <- function(.tbl, col, replacement){

  #---- Checks

  # col in .tbl
  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(.tbl, col_name, ".tbl", "col")

  # replacement type string
  if (typeof(.tbl[[col_name]]) != typeof(replacement)) {abort_bad_argument("replacement", "be the same type as `col`", not = replacement, arg2 = "col", .tbl[[col_name]])}

  mutated <- .tbl |>
    dplyr::mutate("{{ col }}" := ifelse(is.na({{ col }}) | is.null({{ col }}), replacement, {{ col }}))

  return(mutated)
}

#' @noRd
admin1_f <- function(){
  tibble::tibble(
    admin1 = c("grand_anse", "sud_est", "sud", "nippes", "ouest", "artibonite", "centre", "nord", "nord_ouest", "nord_est"),
    admin1_name = c("Grand'Anse", "Sud Est", "Sud", "Nippes", "Ouest", "Artibonite", "Centre", "Nord", "Nord Ouest", "Nord Est"),
    admin1_upper = c("GRAND'ANSE", "SUD EST", "SUD", "NIPPES", "OUEST", "ARTIBONITE", "CENTRE", "NORD", "NORD OUEST", "NORD EST")
  )
 }
