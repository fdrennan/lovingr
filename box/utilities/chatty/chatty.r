#' @export
chatty <- function(input) {
  box::use(jsonlite, shiny)
  json_data <- jsonlite$toJSON(shiny$reactiveValuesToList(input), pretty = TRUE)
  shiny$showNotification(
    json_data
  )
}
