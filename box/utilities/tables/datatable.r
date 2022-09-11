#' @export
ui_dt <- function(id = "dt") {
  box::use(shiny, DT)
  ns <- shiny$NS(id)
  DT$DTOutput(ns("ui"))
}

#' @export
server_dt <- function(id = "dt", data) {
  box::use(shiny, DT)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- DT$renderDT({
        DT::datatable(data)
      })
    }
  )
}
