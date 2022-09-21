#' histogramUI
#' @export histogramUI
histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

#' histogramServer
#' @export histogramServer
histogramServer <- function(input, output, session) {
  data <- reactive(mtcars[[input$var]])
  output$hist <- renderPlot(
    {
      hist(data(), breaks = input$bins, main = input$var)
    },
    res = 96
  )
}
server <- function(input, output, session) {
  callModule(histogramServer, "hist1")
}
