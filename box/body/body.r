

#' @export
ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / utilities / tables / datatable)
  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    bs4Dash$tabItems(
      bs4Dash$tabItem(
        tabName = "tab1",
        file_upload$ui_file_upload(ns("file_upload"))
      ),
      bs4Dash$tabItem(
        tabName = "tab2",
        datatable$ui_dt(ns("config"), collapsed = FALSE),
        xlsx$ui_xlsx(ns("xlsx"))
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  box::use(shiny, bs4Dash)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / csm_config / clean)
  box::use(.. / utilities / tables / datatable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      datapath <- file_upload$server_file_upload("file_upload")
      config <- xlsx$server_xlsx("xlsx", datapath)
      shiny$observeEvent(datapath(), {
        bs4Dash$updateTabItems(
          session = appSession,
          inputId = "sidebar",
          selected = "tab2"
        )
      })
      shiny$observe({
        shiny$req(config())
        datatable$server_dt("config", clean$clean_config(config()))
      })
    }
  )
}
