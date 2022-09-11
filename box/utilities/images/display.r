#' @export
ui_image_output <- function(id = "image") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$imageOutput(ns("image"))
}

#' @export
server_image_output <- function(
  id = "image", datapath, session
) {
  box::use(shiny)
  
  server_fn <- function(
    input, output, session
  ) {
    output$image <- shiny$renderImage({
      browser()
      shiny$req(datapath)
      list(src = datapath)
    })
  }
  
  shiny$moduleServer(
    id,
    server_fn, 
    session
  )
}
