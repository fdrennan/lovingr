

#' @export
ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash, shinyFiles, fs)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / options / options)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)

  box::use(sortable)
  analysis_code_files <- fs$dir_ls(
    "box/analysis/execute",
    recurse = TRUE, type = "file"
  )
  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$includeCSS("www/styles.css"),
    shiny$fluidRow(
      id = "mainSort", sortable$sortable_js("mainSort"),
      options$ui_options(ns("options"), width = 12),
      bs4Dash$box(
        title = "Code Review", width = 12, closable = TRUE, collapsed = TRUE,
        collapsible = TRUE, maximizable = TRUE,
        shiny$fluidRow(
          shiny$column(
            8,
            shiny$wellPanel(
              shiny$selectizeInput(
                ns("filePathForDisplay"), "File",
                choices = analysis_code_files, selected = NULL, multiple = TRUE
              )
            )
          ),
          shiny$column(
            4,
            shiny$wellPanel(
              shiny$numericInput(
                ns("widthOfCols"),
                label = "Col Width",
                value = 4, min = 1, max = 12, step = 1
              )
            )
          ),
          shiny$uiOutput(ns("codeUIEditor"), container = function(...) {
            shiny$column(12, shiny$fluidRow(...))
          })
        )
      ),
      bs4Dash$box(
        closable = TRUE,
        id = ns("importBox"),
        status = "primary",
        maximizable = TRUE,
        title = "Data Import", width = 12,
        shiny$fluidRow(
          shiny$div(
            class = "col-xl-6 col-lg-6 col-md-12",
            shiny$fluidRow(
              metadata$ui_metadata(ns("metadata"), width = 12)
            )
          ),
          shiny$div(
            class = "col-xl-6 col-lg-6 col-md-12",
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
      shiny$uiOutput(ns("codeUI"), container = function(...) {
        shiny$column(12, ...)
      }),
      shiny$uiOutput(ns("dataRaw"), container = function(...) {
        shiny$column(12, ...)
      }),
      shiny$uiOutput(ns("scoreboard"), container = function(...) {
        shiny$column(12, ...)
      })
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  box::use(
    shiny, uuid, bs4Dash, dplyr, shinyFiles,
    fs, utils, purrr, shinyAce, jsonlite
  )
  box::use(.. / utilities / chatty / chatty)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / read / xlsx)
  box::use(.. / csm_config / clean)
  box::use(.. / analysis / app / run_analysis / run_analysis)
  box::use(.. / utilities / tables / datatable)
  box::use(.. / metadata / metadata)
  box::use(.. / utilities / io / file_read_multi_ext)
  box::use(.. / utilities / options / options)
  box::use(.. / utilities / read / xlsx)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$codeUIEditor <- shiny$renderUI({
        shiny$req(input$filePathForDisplay)
        lapply(
          input$filePathForDisplay,
          function(path) {
            shiny$column(
              input$widthOfCols,
              shiny$tags$p(fs$path_file(path)),
              shinyAce$aceEditor(
                outputId = uuid$UUIDgenerate(),
                value = file_read_multi_ext$run(path),
                mode = "r",
                hotkeys = list(
                  helpKey = "F1",
                  runKey = list(
                    win = "Ctrl-R|Ctrl-Shift-Enter",
                    mac = "CMD-ENTER|CMD-SHIFT-ENTER"
                  )
                ),
                wordWrap = TRUE, debounce = 10
              )
            )
          }
        )
      })

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
        clean_config
      })

      output$dataRaw <- shiny$renderUI({
        shiny$req(metadata())
        shiny$req(clean_config())
        shiny$fluidRow(
          shiny$div(class = "col-xl-1 col-lg-1"),
          shiny$div(
            class = "col-xl-5 col-lg-5 col-md-12 col-sm-12",
            shiny$fluidRow(
              bs4Dash$box(
                closable = TRUE,
                maximizable = TRUE,
                title = "Configuration", width = 12,
                collapsed = TRUE,
                status = "info",
                xlsx$ui_xlsx(ns("xlsx-local"))
              )
            )
          ),
          shiny$div(
            class = "col-xl-5 col-lg-5 col-md-12 col-sm-12",
            shiny$fluidRow(
              bs4Dash$box(
                closable = TRUE,
                collapsed = TRUE,
                maximizable = TRUE,
                width = 12,
                status = "primary",
                title = "Data Preview",
                shiny$fluidRow(id = "dataPreview")
              )
            )
          ),
          shiny$div(class = "col-xl-1 col-lg-1"),
          bs4Dash$box(
            width = 12,
            title = "Flagging Results and Review",
            shiny$div(id = "uiAnalyses")
          ),
          shiny$column(
            class = "d-flex justify-content-end",
            12, shiny$actionButton(ns("getResults"), "Get Results")
          )
        )
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
          shiny$div(
            class = "col-xl-12 col-lg-12 col-md-12 col-sm-12",
            id = "dataPreviewElements"
          )
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

      dataForScoreboard <-
        shiny$reactive({
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

          output <- purrr$imap(
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
              output <- run_analysis$server_run_analysis(
                paste0("run_analysis", name), analysis_data, variables
              )
              output()
            }
          )

          output
        })

      shiny$observeEvent(
        input$getResults,
        {
          dataForScoreboard <- dataForScoreboard()
          analysisStatistics <-
            purrr$map_dfr(dataForScoreboard, ~ .$analysisStatistics)
          output$scoreboard <- shiny$renderUI({
            datatable$ui_dt(ns("analysisStatistics"))
          })
          datatable$server_dt("analysisStatistics", data = analysisStatistics)
        }
      )
    }
  )
}
