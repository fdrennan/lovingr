
#' @export
ui_file_upload <- function(id = "file_upload",
                           width = 6,
                           label = "Upload a file",
                           accept = "*",
                           multiple = FALSE,
                           ...) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    title = "File Upload",
    width = width,
    shiny$fileInput(
      inputId = ns("fileUpload"),
      label = label,
      accept = accept,
      multiple = multiple
    ),
    shiny$tableOutput(ns("fileMetaData")),
    ...
  )
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

      out <- shiny$reactive({
        if (getOption("development")) {
          out <- getOption("base_config")
        } else {
          shiny$req(input$fileUpload)
          out <- input$fileUpload$datapath
        }
        out
      })

      out
    }
  )
}
