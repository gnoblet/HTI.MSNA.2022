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
label_format <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 3,
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
      paste0(prefix, formatNum(cuts[-n] + 1), between, formatNum(cuts[-1]),
             suffix)
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
leaflet_color_bin <- function(pal = visualizeR::pal_reach("red_alt"), domain, bins = 5, na_color = visualizeR::cols_reach("white"), right = TRUE, reverse = FALSE){
  leaflet::colorBin(
    pal,
    domain = domain,
    bins = bins,
    na.color = na_color,
    right = TRUE
  )
}
