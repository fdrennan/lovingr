

#' @export
ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / options / options)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)

  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$fluidRow(
      shiny$column(
        8,
        offset = 2,
        shiny$fluidRow(
          options$ui_options(ns("options"), width = 12)
        ),
        shiny$fluidRow(
          bs4Dash$box(
            title = "Data Import", width = 12,
            metadata$ui_metadata(ns("metadata"), width = 12),
            file_upload$ui_file_upload(ns("file_upload"),
              width = 12,
              footer = shiny$div(
                class = "text-right",
                {
                  if (getOption("development")) {
                    shiny$tags$p("Running in development mode.")
                  }
                }
              )
            )
          )
        ),
        shiny$fluidRow(
          bs4Dash$box(
            title = "Raw Data", width = 12,
            shiny$fluidRow(
              shiny$column(
                12,
                xlsx$ui_xlsx(ns("xlsx"))
              )
            )
          )
        ),
        shiny$fluidRow(
          datatable$ui_dt(
            ns("config"),
            title = "Flagging Preview",
            collapsed = FALSE, width = 12
          )
        )
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  box::use(shiny, bs4Dash, dplyr)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / csm_config / clean)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)
  box::use(.. / utilities / options / options)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      opts <- options$server_options("options")

      shiny$observe({
        browser()
        shiny$req(opts)
        shiny$showNotification("Ops updated")
      })
      metadata <- metadata$server_metadata("metadata")
      datapath <- file_upload$server_file_upload("file_upload")
      config <- xlsx$server_xlsx("xlsx", datapath, width = 12)



      shiny$observe({
        shiny$throttle(metadata(), 20000)
        shiny$throttle(config(), 20000)

        shiny$req(metadata())
        shiny$req(config())


        metadata <- metadata()
        config <- config()


        clean_config <- clean$clean_config(config)
        clean_config <- dplyr$left_join(metadata, clean_config)

        clean_config_subset <- dplyr$select(
          clean_config, analysis, paramcd, flagging_specification
        )
        datatable$server_dt("config", clean_config_subset)
      })
    }
  )
}
