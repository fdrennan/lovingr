#' @export
ui_footer <- function(id = "footer") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$bs4DashFooter()
}
