
#' @export
ui_file_upload <- function(id = "file_upload",
                           width = 6,
                           label = NULL,
                           accept = "xlsx",
                           multiple = FALSE) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
}

#' @export
server_file_upload <- function(id = "file_upload",
                               display_meta = FALSE) {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      if (display_meta) {
        shiny$observeEvent(input$fileUpload, {
          output$fileMetaData <- shiny$renderTable({
            as.data.frame(input$fileUpload)
          })
        })
      }
    }
  )
}
