
#' @export
ui_file_upload <- function(id = "file_upload", 
                           label = 'Upload a file', accept = "*", multiple = FALSE) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    bs4Dash$box(
      title = "File Upload",
      width = 12,
      shiny$fileInput(
        inputId = ns("fileUpload"),
        label = label,
        accept = accept,
        multiple = multiple
      ),
      shiny$tableOutput(ns("fileMetaData"))
    )
  )
}

#' @export
server_file_upload <- function(id = "file_upload") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shiny$observeEvent(input$fileUpload, {
        output$fileMetaData <- shiny$renderTable({
          as.data.frame(input$fileUpload)
        })
      })

      out <- shiny$reactive({
        shiny$req(input$fileUpload)
        input$fileUpload$datapath
      })

      out
    }
  )
}
