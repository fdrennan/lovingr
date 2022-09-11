#' @export
ui_header <- function(id = "header") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardHeader(skin = "dark")
}
