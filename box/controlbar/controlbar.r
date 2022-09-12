#' @export
ui_controlbar <- function(id = "controlbar") {
  box::use(shiny, bs4Dash)

  box::use(.. / devop / devop)
  ns <- shiny$NS(id)
  print(ns("metadata"))
  bs4Dash$dashboardControlbar(
    collapsed = TRUE,
    shiny$div(
      class = "p-3",
      if (getOption("development")) devop$ui_devop(ns("devop")) else NULL
    )
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
