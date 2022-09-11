
#' @export
ui_file_upload <- function(id='file_upload') {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns('ui'))
  
}

#' @export
server_file_upload <- function(id='file_upload') {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
    ns <- session$ns
    output$ui <- shiny$renderUI({
        shiny$div('file_upload')
      })
    })
}