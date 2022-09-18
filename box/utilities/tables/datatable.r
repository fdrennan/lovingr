#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = TRUE,
                  width = 12, status = "secondary") {
  box::use(shiny, DT, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    closable = TRUE,
    maximizable = TRUE,
    width = width,
    status = status,
    solidHeader = TRUE,
    # background = "white",
    title = title, collapsed = collapsed,
    shiny$fluidRow(shiny$column(12, DT$DTOutput(ns("ui"), width = "100%")))
  )
}

#' @export
server_dt <- function(id = "dt", data, pageLength = 3) {
  box::use(shiny, DT, bs4Dash)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- DT$renderDT(
        server = TRUE,
        {
          DT::datatable(data,
            options = list(
              scrollX = TRUE,
              pageLength = pageLength,
              filter = "top"
            ),
            class = "compact",
            caption = NULL,
            filter = c("none", "bottom", "top"),
            escape = TRUE,
            style = "bootstrap4",
            width = NULL,
            height = NULL,
            elementId = NULL,
            fillContainer = getOption("DT.fillContainer", NULL),
            autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
            selection = "none", #  c("multiple", "single", "none"),
            extensions = list(),
            plugins = NULL,
            editable = FALSE
          )
        }
      )
    }
  )
}
