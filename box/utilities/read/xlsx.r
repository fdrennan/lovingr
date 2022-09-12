
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

      xlsx_data <- shiny$eventReactive(datapath(), {
        datapath <- datapath()
        datapath_ext <- fs$path_ext(datapath)
        is_xlsx <- datapath_ext == "xlsx"
        if (isFALSE(is_xlsx)) {
          shiny$showNotification(glue$glue(
            "{datapath} must be an xlsx file"
          ))
          shiny$req(is_xlsx)
        }

        sheetNames <- openxlsx$getSheetNames(datapath)

        lapply(sheetNames, function(sheetName) {
          list(
            sheetName = sheetName,
            data = openxlsx$read.xlsx(datapath, sheetName)
          )
        })
      })

      shiny$observeEvent(xlsx_data(), {
        xlsx_data <- xlsx_data()
        lapply(
          xlsx_data,
          function(data) {
            uuid <- uuid::UUIDgenerate()
            shiny$insertUI(
              "#sheets",
              "afterEnd",
              datatable$ui_dt(ns(uuid), title = data$sheetName)
            )
            datatable$server_dt(uuid, data$data)
          }
        )
      })

      xlsx_data
    }
  )
}
