ui_xlsx <- function(id = "xlsx") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

server_xlsx <- function(id = "xlsx") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$ui <- shiny$renderUI({
        
        shiny$sidebarLayout(
          shiny$sidebarPanel(
            shiny$fileInput(ns("file"), "Choose CSV File", accept = ".xlsx"),
            shiny$actionButton(ns('submit'), 'Submit')
          ),
          shiny$mainPanel(
            shiny$tableOutput(ns("contents"))
          )
        )
      })

      output$contents <- shiny$renderTable({
        shiny$req(input$submit)
        file <- input$file
        ext <- tools::file_ext(file$datapath)

        shiny$req(file)
        shiny$validate(shiny$need(
          ext == "xlsx", "Please upload a xlsx file"
        ))

        openxlsx$read.xlsx(file$datapath)
      })
    }
  )
}



ui <- function() {
  shiny$fluidPage(
    shiny$fluidRow(
      ui_xlsx()
    )
  )
}
server <- function(input, output, session) {
  server_xlsx()
}

box::use(shiny)
shiny$shinyApp(ui, server)
