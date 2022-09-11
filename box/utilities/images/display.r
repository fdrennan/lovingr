#' @export
ui_image_output <- function(id = "image") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  shiny$imageOutput(ns("image"))
}

#' @export
server_image_output <- function(id = "image", datapath) {
  box::use(shiny)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      output$image <- shiny$renderImage({
        shiny$req(datapath)
        list(src = datapath, className = paste0("img-fluid"))
      })
    },
  )
}
