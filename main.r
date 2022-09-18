ui <- function() {
  box::use(. / box / app)
  app$ui_app()
}

server <- function(input, output, session) {
  box::use(. / box / app)
  app$server_app(session)
}

box::use(shiny)
shiny$shinyApp(ui, server)
