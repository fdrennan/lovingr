#' @export
ui <- function(id = "metadata") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  shiny$wellPanel(
    shiny$uiOutput(ns("study")),
    shiny$uiOutput(ns("year")),
    shiny$uiOutput(ns("month")),
    shiny$uiOutput(ns("analysis"))
  )
}
