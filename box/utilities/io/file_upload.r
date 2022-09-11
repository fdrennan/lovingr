
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
        options(shiny.maxRequestSize = 300 * 1024^2)
        shiny$fileInput(
          ns("fileUpload"),
          "Choose CSV File",
          accept = "*",
          multiple = TRUE
        )
      },
      shiny$tableOutput(ns("fileMetaData")),
      shiny$div(id = ns("image_container"))
    )
  )
}

#' @export
server_file_upload <- function(id = "file_upload") {
  box::use(shiny, bs4Dash, ../images/display)
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

      shiny$observeEvent(file(), {
        file <- file()
        lapply(
          split(file, 1:nrow(file)), function(x) {
            shiny$insertUI(
              selector = paste0("#", ns("image_container")),
              where = 'afterBegin',
              ui = display$ui_image_output(x$name)
            )
          }
        )
        
        lapply(
          split(file, 1:nrow(file)), 
          function(x) {
            browser()
            display$server_image_output(x$name, x$datapath, session)
          }
        )
        
      })
    }
  )
}
