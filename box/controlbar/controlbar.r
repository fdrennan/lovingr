#' @export
ui_controlbar <- function(id = "controlbar") {
  box::use(shiny, bs4Dash)
  box::use(.. / devop / devop)
  ns <- shiny$NS(id)
  bs4Dash$dashboardControlbar(
    collapsed = TRUE,
    shiny$div(class = "py-3", devop$ui_devop(ns("devop")))
  )
}

#' @export
server_controlbar <- function(id = "controlbar") {
  box::use(shiny)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      box::use(.. / devop / devop)
      devop$server_devop(id = "devop")
    }
  )
}
