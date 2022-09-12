#' @export
ui <- function(id = "devop") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$inputPanel(
    shiny$actionButton(
      "deleteDevelopmentFolder",
      "Delete Develoment Folder"
    ),
    shiny$actionButton(
      "createDevelopmentFolder",
      "Create Develoment Folder"
    )
  )
}
