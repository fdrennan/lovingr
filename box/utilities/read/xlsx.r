
#' @export
ui_xlsx <- function(id = "xlsx") {
  box::use(shiny, .. / tables / datatable, sortable)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(12, shiny$fluidRow(id = "sheets")),
    sortable$sortable_js(css_id = "sheets")
  )
}

#' @export
server_xlsx <- function(id = "xlsx", datapath, width = 12, ui_id = "#sheets") {
  box::use(shiny, openxlsx, fs, glue, uuid)
  box::use(shiny, .. / tables / datatable, haven, readr)
  shiny$moduleServer(
    id,
    # session = parentSession,
    function(input, output, session) {
      ns <- session$ns

      xlsx_data <- shiny$reactive({
        shiny$req(datapath)
        datapath_ext <- fs$path_ext(datapath)

        out <- switch(datapath_ext,
          "xlsx" = {
            sheetNames <- openxlsx$getSheetNames(datapath)
            lapply(sheetNames, function(sheetName) {
              list(
                sheetName = sheetName,
                data = openxlsx$read.xlsx(datapath, sheetName)
              )
            })
          },
          "sas7bdat" = list(
            list(
              sheetName = fs$path_file(datapath),
              data = haven$read_sas(datapath)
            )
          ),
          "csv" = {
            list(
              list(
                sheetName = fs$path_file(datapath),
                data = readr$read_csv(datapath)
              )
            )
          }
        )
        out
      })


      shiny$observeEvent(xlsx_data(), {
        xlsx_data <- xlsx_data()
        lapply(
          xlsx_data,
          function(data) {
            uuid <- uuid::UUIDgenerate()
            shiny$insertUI(
              ui_id,
              "afterBegin",
              datatable$ui_dt(
                ns(uuid),
                width = width,
                title = data$sheetName
              )
            )

            out <- datatable$server_dt(uuid, data$data, title = data$sheetName)
            out
          }
        )
      })

      xlsx_data
    }
  )
}
