
#' @export
ui_xlsx <- function(id = "xlsx") {
  box::use(shiny, .. / tables / datatable)
  ns <- shiny$NS(id)
  shiny$div(id='sheets')
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
        sheetNames <- openxlsx$getSheetNames(datapath)
        n_sheets <- seq_along(sheetNames)
        lapply(
          n_sheets,
          function(x) {
            shiny$insertUI(
              '#sheets', 
              'afterBegin',
              datatable$ui_dt(ns(x))
            )
            data <- openxlsx$read.xlsx(datapath, sheetNames[[x]])
            datatable$server_dt(x, data)
          }
        )
      })
    }
  )
}
