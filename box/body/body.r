

#' @export
ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash, shinyFiles)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / options / options)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)


  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$includeCSS("www/styles.css"),
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
              footer = if (getOption("development")) {
                shiny$tags$p("Upload Disabled - Running in development mode.")
              }
            )
          )
        ),
        shiny$fluidRow(
          bs4Dash$box(
            title = "Raw Data", width = 12,
            shiny$fluidRow(
              shiny$column(
                12,
                xlsx$ui_xlsx(ns("xlsx-server")),
                xlsx$ui_xlsx(ns("xlsx-local"))
              )
            )
          )
        ),
        shiny$fluidRow(
          datatable$ui_dt(
            ns("clean_config"),
            title = "Analysis Data",
            collapsed = TRUE, width = 12
          )
        ),
        shiny$fluidRow(id = "dataPreview")
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  box::use(shiny, bs4Dash, dplyr, shinyFiles)
  box::use(.. / utilities / chatty / chatty)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / csm_config / clean)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)
  box::use(.. / utilities / options / options)
  box::use(.. / utilities / read / xlsx)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny$observe(chatty$chatty(session, input))

      opts <- options$server_options("options")

      metadata <- metadata$server_metadata("metadata")

      shinyFiles$shinyFileChoose(
        input, "files",
        root = c(root = {
          "."
        }),
        filetypes = c("xlsx")
      )


      datapathUpload <- file_upload$server_file_upload("file_upload")


      config <- shiny$eventReactive(datapathUpload, {
        shiny$showNotification("Using Uploaded File")
        datapathUpload <- datapathUpload()
        xlsx$server_xlsx("xlsx-local", datapathUpload, width = 12)
      })

      clean_config <- shiny$reactive({
        shiny$req(metadata())
        shiny$req(config()())
        clean_config <- clean$clean_config(config()())
        clean_config <- dplyr$left_join(metadata(), clean_config)
        datatable$server_dt("clean_config", clean_config)
        clean_config
      })

      shiny$observeEvent(clean_config(), {
        clean_config <- clean_config()
        import_files <- dplyr$distinct(clean_config(), analysis, filepath)
        uuid <- uuid::UUIDgenerate()
        lapply(
          import_files$filepath,
          function(path) {
            shiny$insertUI(
              "#dataPreview",
              "afterBegin",
              xlsx$ui_xlsx(ns(uuid))
            )
            xlsx$server_xlsx(uuid, datapath = path, ui_id = "#dataPreview")
          }
        )
      })
    }
  )
}

# shinyFiles$shinyFilesButton(ns('files'),
#                             label='File select',
#                             title='Please select a file', multiple=FALSE)
# datapathServer <- shiny$reactive({
#   input$files$files[[1]][[2]]
# })
#
#
# config <- shiny$eventReactive(datapathServer, {
#   browser
#   shiny$showNotification('Using Server File')
#   xlsx$server_xlsx("xlsx-server", datapathServer, width = 12)
# })
#
