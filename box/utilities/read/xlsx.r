
#' @export
ui_xlsx <- function(id = "xlsx") {
  box::use(shiny, .. / tables / datatable)
  ns <- shiny$NS(id)
  shiny$fluidRow(id = "sheets")
}

#' @export
server_xlsx <- function(id = "xlsx", datapath) {
  box::use(shiny, openxlsx, fs, glue, uuid)
  box::use(shiny, .. / tables / datatable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      xlsx_path <- shiny$eventReactive(datapath(), {
        datapath <- datapath()
        datapath_ext <- fs$path_ext(datapath)
        is_xlsx <- datapath_ext == "xlsx"
        if (isFALSE(is_xlsx)) {
          shiny$showNotification(glue$glue(
            "{datapath} must be an xlsx file"
          ))
          shiny$req(is_xlsx)
        }

        datapath
      })

      shiny$observeEvent(xlsx_path(), {
        datapath <- datapath()
        sheetNames <- openxlsx$getSheetNames(datapath)
        lapply(
          sheetNames,
          function(sheetName) {
            uuid <- uuid::UUIDgenerate()
            shiny$insertUI(
              "#sheets",
              "afterBegin",
              datatable$ui_dt(ns(uuid), title = sheetName)
            )
            data <- openxlsx$read.xlsx(datapath, sheetName)
            datatable$server_dt(uuid, data)
          }
        )
      })
    }
  )
}
