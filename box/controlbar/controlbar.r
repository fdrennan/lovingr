#' @export
ui_controlbar <- function(id = "controlbar") {
  box::use(shiny, bs4Dash)
  box::use(.. / metadata / ui_metadata)
  box::use(.. / devop / ui_devop)
  ns <- shiny$NS(id)
  bs4Dash$dashboardControlbar(
    collapsed = TRUE,
    shiny$div(
      class = "p-3",
      if (getOption("development")) NULL else ui_devop$ui(),
      ui_metadata$ui()
    )
  )
}

#' @export
server_controlbar <- function(id) {
  box::use(shiny)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      box::use(.. / metadata / ui_metadata)
      box::use(.. / devop / ui_devop)
      server_devop$server()
      metadata <- server_metadata$server()

      observe({
        browser()
        shiny$req(metadata())
        print(metadata())
      })
    }
  )
}
