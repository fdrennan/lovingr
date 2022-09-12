#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = TRUE, width = 12) {
  box::use(shiny, DT, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    width = width,
    title = title, collapsed = collapsed,
    DT$DTOutput(ns("ui"))
  )
}

#' @export
server_dt <- function(id = "dt", data, pageLength = 50) {
  box::use(shiny, DT, bs4Dash)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- DT$renderDT({
        DT::datatable(data,
          options = list(
            scrollX = TRUE,
            pageLength = pageLength,
            filter = "top"
          ),
          class = "display",
          caption = NULL,
          filter = c("none", "bottom", "top"),
          escape = TRUE,
          style = "auto",
          width = NULL,
          height = NULL,
          elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = c("multiple", "single", "none"),
          extensions = list(),
          plugins = NULL,
          editable = FALSE
        )
      })
    }
  )
}
