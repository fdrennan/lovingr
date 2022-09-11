
#' @export
ui_xlsx <- function(id = "xlsx") {
  box::use(shiny, .. / tables / datatable)
  ns <- shiny$NS(id)
  datatable$ui_dt(ns("dt"))
}

#' @export
server_xlsx <- function(id = "xlsx", datapath) {
  box::use(shiny, openxlsx)
  box::use(shiny, .. / tables / datatable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      path <- shiny$observeEvent(datapath(), {
        datapath <- datapath()
        data <- openxlsx$read.xlsx(datapath)
        datatable$server_dt("dt", data)
      })
    }
  )
}
