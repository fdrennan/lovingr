#' @export
ui_controlbar <- function(id = "controlbar") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardControlbar(
    collapsed = TRUE
  )
}
