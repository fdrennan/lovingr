#' plotImageOutput
#' @export plotImageOutput
plotImageOutput <- function(data, label = "Counter") {
  id <- as.character(data)
  ns <- NS(id)
  # cli_alert_info('plotImageOutput id is {id}')
  imageOutput(ns("plot"))
}

#' plotImageServer
#' @export plotImageServer
plotImageServer <- function(data, height = "auto", width = "100%") {
  moduleServer(
    data,
    function(input, output, session) {
      output$plot <- renderImage(
        {
          list(
            src = data,
            contentType = "image/png",
            height = height,
            width = width,
            alt = data
          )
        },
        deleteFile = FALSE
      )
    }
  )
}
