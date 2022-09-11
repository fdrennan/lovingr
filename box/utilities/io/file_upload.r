
#' @export
ui_file_upload <- function(id = "file_upload") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    title = "File Upload",
    width = 12,
    dropdownMenu = bs4Dash$boxDropdown(
      icon = shiny$icon("info")
    ),
    sidebar = bs4Dash$boxSidebar(
      startOpen = FALSE
    ),
    shiny$fluidRow(
      class = "p-2",
      {
        shiny$fileInput(
          ns("fileUpload"),
          "Choose CSV File",
          accept = "*",
          multiple = TRUE
        )
      },
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
