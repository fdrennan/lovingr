

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
        12,
        # offset = 1,
        shiny$fluidRow(
          options$ui_options(ns("options"), width = 12)
        ),
        shiny$fluidRow(
          bs4Dash$box(
            id = ns("importBox"),
            status = "primary",
            maximizable = TRUE,
            title = "Data Import", width = 12,
            shiny$fluidRow(
              shiny$div(
                class = "col-xl-6 col-lg-12 col-md-12",
                shiny$fluidRow(
                  metadata$ui_metadata(ns("metadata"), width = 12)
                )
              ),
              shiny$div(
                class = "col-xl-6 col-lg-12 col-md-12",
                shiny$fluidRow(
                  file_upload$ui_file_upload(ns("file_upload"),
                    width = 12,
                    footer = if (getOption("development")) {
                      shiny$tags$p("Upload Disabled - Running in development mode.")
                    }
                  )
                )
              ),
              shiny$column(12,
                class = "text-right py-3",
                bs4Dash$actionButton(ns("start"), "Start", status = "primary")
              )
            )
          ),
          bs4Dash$box(
            maximizable = TRUE,
            title = "Configuration", width = 12,
            collapsed = FALSE,
            status = "info",
            xlsx$ui_xlsx(ns("xlsx-server")),
            xlsx$ui_xlsx(ns("xlsx-local"))
          ),
          datatable$ui_dt(
            ns("clean_config"),
            status = "info",
            title = "Flagging Summary",
            collapsed = TRUE, width = 12
          ),
          bs4Dash$box(
            collapsed = TRUE,
            maximizable = TRUE,
            width = 12,
            status = "primary",
            title = "Data Preview",
            shiny$div(id = "dataPreview")
          )
        ),
        shiny$fluidRow(
          shiny$column(12, id = "uiAnalyses")
        )
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  box::use(shiny, bs4Dash, dplyr, shinyFiles, fs, utils, purrr)
  box::use(.. / utilities / chatty / chatty)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / csm_config / clean)
  box::use(.. / analysis / app / run_analysis / run_analysis)
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

      datapathUpload <- file_upload$server_file_upload("file_upload")


      config <- shiny$eventReactive(datapathUpload, {
        datapathUpload <- datapathUpload()
        xlsx$server_xlsx("xlsx-local", datapathUpload, width = 12)
      })

      clean_config <- shiny$eventReactive(input$start, {
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


        shiny$removeUI(
          "#dataPreviewElements"
        )

        shiny$insertUI(
          "#dataPreview",
          "afterBegin",
          shiny$fluidRow(id = "dataPreviewElements")
        )

        lapply(
          import_files$filepath,
          function(path) {
            shiny$insertUI(
              "#dataPreviewElements",
              "afterBegin",
              xlsx$ui_xlsx(ns(uuid))
            )
            xlsx$server_xlsx(
              uuid,
              datapath = path,
              ui_id = "#dataPreviewElements", width = 12
            )
          }
        )
      })

      shiny$observeEvent(clean_config(), {

        # shiny$req(clean_config())
        clean_config <- clean_config()
        analysis_code <- fs$dir_info("box/analysis/execute")
        analysis_code <- analysis_code |>
          dplyr$select(path) |>
          dplyr$mutate(
            analysis = gsub("box/analysis/execute/analysis_", "", path),
            analysis = fs$path_ext_remove(analysis)
          )

        analysis_code <- dplyr$inner_join(analysis_code, clean_config)
        analysis_code <- split(analysis_code, analysis_code$analysis)
        n_increments <- length(analysis_code)
        purrr$iwalk(
          analysis_code,
          function(analysis_data, name) {
            shiny$insertUI(
              "#uiAnalyses",
              "afterBegin",
              run_analysis$ui_run_analysis(
                ns(paste0("run_analysis", name)), analysis_data
              )
            )

            variables <- dplyr$mutate_all(config()()[[1]]$data, tolower)
            names(variables) <- tolower(names(variables))
            run_analysis$server_run_analysis(
              paste0("run_analysis", name), analysis_data, variables
            )
          }
        )
      })
    }
  )
}
