

#' @export
ui_body <- function(id = "body") {
  # Imports
  {
    box::use(shiny, bs4Dash, shinyFiles, fs, shinyjs)
    box::use(.. / utilities / options / options)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / tables / datatable)
    box::use(.. / metadata / metadata)
  }

  box::use(sortable)

  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$includeCSS("www/styles.css"),
    shinyjs$useShinyjs(),
    shiny$fluidRow(
      shiny$column(
        class = "d-flex justify-content-end align-items-top pb-3",
        12, bs4Dash$actionButton(ns("resetPage"), "Start Over", size = "xs")
      ),
      shiny$uiOutput(ns("mainUI"), container = function(...) {
        shiny$column(12, ...)
      })
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  # Imports
  {
    box::use(shiny, uuid, bs4Dash, glue, dplyr, tidyselect, shinyFiles)
    box::use(fs, utils, purrr, shinyAce, jsonlite, shinyjs)
    box::use(.. / utilities / chatty / chatty)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / analysis / app / run_analysis / run_analysis)
    box::use(.. / utilities / tables / datatable)
    box::use(mda = .. / metadata / metadata)
    box::use(.. / utilities / io / file_read_multi_ext)
    box::use(.. / utilities / options / options)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / codereview / codereview)
  }

  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$mainUI <- shiny$renderUI({
      out <-
        shiny$fluidRow(
          mda$ui_metadata(ns("metadata"), width = 12),
          shiny$uiOutput(ns("metaDataReviewUI"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$uiOutput(ns("dataRaw"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$uiOutput(ns("analysisUI"), container = function(...) {
            shiny$column(12, shiny$fluidRow(...))
          }),
          shiny$uiOutput(ns("scoreboard"), container = function(...) {
            shiny$column(12, ...)
          })
        )

      out
    })
    shiny$observeEvent(input$resetPage, {
      shinyjs$refresh()
    })
    metadata <- mda$server_metadata("metadata")
    shiny$observe({
      shiny$req(metadata())
      output$metaDataReviewUI <- shiny$renderUI({
        shiny$fluidRow(
          datatable$ui_dt(ns("metaDataReview"), "Meta Data Review", collapsed = TRUE)
        )
      })
      datatable$server_dt("metaDataReview", metadata()$clean)
    })
    #
    dataFiles <- shiny$eventReactive(metadata(), {
      bs4Dash$updateBox("metaDataReview", action = "toggle")
      metadata <- metadata()
      import_files <- dplyr$distinct(metadata()$clean, analysis, filepath)
      uuid <- uuid::UUIDgenerate()

      output$dataRaw <- shiny$renderUI({
        shiny$fluidRow(
          shiny$column(12, id = "datamiscFilesRaw"),
          shiny$column(12, class = "text-right py-3", shiny$actionButton(ns("proceedToStartAnalysis"), "Start Analyses"))
        )
      })

      shiny$removeUI("#datamiscFilesRawElements")
      shiny$insertUI(
        "#datamiscFilesRaw", "afterBegin",
        shiny$fluidRow(
          id = "datamiscFilesRawElements"
        )
      )
      variables <- dplyr$mutate_all(metadata()$raw[[1]]$data, tolower)
      #
      output <- purrr$map2(import_files$filepath, import_files$analysis, function(path, analysis) {
        shiny$insertUI("#datamiscFilesRawElements", "afterBegin", xlsx$ui_xlsx(ns(uuid)))
        out <- xlsx$server_xlsx(uuid, datapath = path, ui_id = "#datamiscFilesRawElements")
        out <- out()[[1]]
        out$analysis <- analysis
        out$metadata <- dplyr$filter(metadata()$clean, analysis == !!analysis)
        out$variables <- variables
        out
      })
      output
    })

    shiny$observe({
      shiny$req(dataFiles())
    })

    shiny$observeEvent(input$proceedToStartAnalysis, {
      output$analysisUI <- shiny$renderUI({
        shiny$column(12, id = "uiAnalyses")
      })
    })

    dataForScoreboard <- shiny$eventReactive(input$proceedToStartAnalysis, {
      output <-
        purrr$map(
          dataFiles(),
          function(analysis_data) {
            shiny$insertUI(
              "#uiAnalyses", "afterBegin",
              run_analysis$ui_run_analysis(
                ns(paste0("run_analysis", analysis_data$analysis)), analysis_data
              )
            )
            #
            output <- run_analysis$server_run_analysis(
              paste0("run_analysis", analysis_data$analysis), analysis_data
            )
            #
            tryCatch(expr = {
              output()
            }, error = function(err) {
            })
          }
        )
      output
    })

    shiny$observeEvent(dataForScoreboard(), {
      scoreboardSheet <- metadata()$raw[[3]]$data |>
        dplyr$rename(analysis = Analysis.Type) |>
        dplyr$mutate(analysis = tolower(analysis))

      flags <- purrr$map_dfr(dataForScoreboard(), function(x) {
        x$flags
      })

      output$scoreboard <- shiny$renderUI({
        shiny$fluidRow(
          datatable$ui_dt(ns("scoreboardConfiguration"), "Scoreboard")
        )
      })
      datatable$server_dt("scoreboardConfiguration", data = flags)
    })
  })
}
