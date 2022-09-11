#' @export
ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash)
  box::use(. / box / utilities / io / file_upload)
  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    bs4Dash$tabItems(
      bs4Dash$tabItem(
        tabName = "tab1",
        file_upload$ui_file_upload()
      )
    )
  )
}
