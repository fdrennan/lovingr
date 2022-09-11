
#' @export
ui_file_upload <- function(id = "file_upload") {
  box::use(shiny, bs4Dash)
  options(shiny.maxRequestSize = 300*1024^2)
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
    shiny$fluidRow(class='p-2',
      shiny$fileInput(
        ns("fileUpload"),
        "Choose CSV File",
        accept = "*",
        multiple = TRUE
      ),
      shiny$tableOutput(ns("fileMetaData"))
    )
  )
}

#' @export
server_file_upload <- function(id = "file_upload") {
  box::use(shiny, bs4Dash)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      file <- shiny$reactive({
        shiny$req(input$fileUpload)
      })

      output$fileMetaData <- shiny$renderTable({
        shiny$req(file())
        as.data.frame(file())
      })
    }
  )
}
