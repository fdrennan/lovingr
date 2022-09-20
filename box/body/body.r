

#' @export
ui_body <- function(id = "body") {
  # Imports
  {
    box::use(shiny, bs4Dash, shinyFiles, fs)
    box::use(.. / utilities / options / options)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / tables / datatable)
    box::use(.. / metadata / metadata)
  }

  box::use(sortable)

  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$includeCSS("www/styles.css"),
    shiny$fluidRow(
      shiny$column(10,
        offset = 1,
        shiny$fluidRow(
          metadata$ui_metadata(ns("metadata"), width = 12),
          shiny$uiOutput(ns("metaDataReviewUI"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$uiOutput(ns("dataRaw"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$uiOutput(ns("analysisUI"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$uiOutput(ns("scoreboard"), container = function(...) {
            shiny$column(12, ...)
          })
        )
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  # Imports
  {
    box::use(shiny, uuid, bs4Dash, glue, dplyr, tidyselect, shinyFiles, fs, utils, purrr, shinyAce, jsonlite)
    box::use(.. / utilities / chatty / chatty)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / analysis / app / run_analysis / run_analysis)
    box::use(.. / utilities / tables / datatable)
    box::use(.. / metadata / metadata)
    box::use(.. / utilities / io / file_read_multi_ext)
    box::use(.. / utilities / options / options)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / codereview / codereview)
  }

  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metadata <- metadata$server_metadata("metadata")

    shiny$observeEvent(metadata(), {
      output$metaDataReviewUI <- shiny$renderUI({
        shiny$fluidRow(
          datatable$ui_dt(ns("metaDataReview"), "Meta Data Review", collapsed = TRUE),
          shiny$column(12, class = "text-right p-3", shiny$actionButton(ns("proceedToDataReview"), "Input Data Review")),
        )
      })
      datatable$server_dt("metaDataReview", metadata()$clean)
    })

    shiny$observeEvent(input$proceedToDataReview, {
      metadata <- metadata()
      import_files <- dplyr$distinct(metadata()$clean, analysis, filepath)
      uuid <- uuid::UUIDgenerate()

      output$dataRaw <- shiny$renderUI({
        shiny$fluidRow(
          shiny$column(12, id = "datamiscFilesRaw"),
          shiny$column(12, class = "text-right p-3", shiny$actionButton(ns("startAnalyses"), "Start Analyses"))
        )
      })

      shiny$removeUI("#datamiscFilesRawElements")
      shiny$insertUI(
        "#datamiscFilesRaw", "afterBegin",
        shiny$fluidRow(
          id = "datamiscFilesRawElements"
        )
      )

      output <- purrr$map(import_files$filepath, function(path) {
        shiny$insertUI("#datamiscFilesRawElements", "afterBegin", xlsx$ui_xlsx(ns(uuid)))
        out <- xlsx$server_xlsx(uuid, datapath = path, ui_id = "#datamiscFilesRawElements")
      })

      output
    })

    shiny$observeEvent(input$startAnalyses, {
      output$analysisUI <- shiny$renderUI({
        shiny$fluidRow(
          shiny$column(12, id = "uiAnalyses")
        )
      })
    })

    dataForScoreboard <- shiny$eventReactive(input$startAnalyses, {
      clean_metadata <- metadata()$clean
      raw_metadata <- metadata()$raw
      clean_metadata <- split(clean_metadata, clean_metadata$analysis)
      n_increments <- length(clean_metadata)

      output <-
        purrr$imap(
          clean_metadata,
          function(analysis_data, name) {
            shiny$insertUI(
              "#uiAnalyses", "afterBegin",
              run_analysis$ui_run_analysis(
                ns(paste0("run_analysis", name)), analysis_data
              )
            )
            variables <- dplyr$mutate_all(raw_metadata[[1]]$data, tolower)
            names(variables) <- tolower(names(variables))
            output <- run_analysis$server_run_analysis(
              paste0("run_analysis", name), analysis_data, variables
            )
            #
            output
          }
        )

      output
    })

    shiny$observeEvent(dataForScoreboard(), {
      scoreboardSheet <- metadata()$raw[[3]]$data |>
        dplyr$rename(
          analysis = Analysis.Type,
          flagging_value = Signal.Flag.Value
        ) |>
        dplyr$mutate(analysis = tolower(analysis))

      dataForScoreboard <- dataForScoreboard()

      dataForScoreboardSummary <-
        purrr$imap_dfr(dataForScoreboard, function(data, analysis) {
          print(analysis)
          print(lapply(data, typeof))
          out <- data$analysisOutput
          out$analysis <- analysis
          out
        })
      scoreboardSheet <- dplyr$inner_join(
        dataForScoreboardSummary, scoreboardSheet
      )

      output$scoreboard <- shiny$renderUI({
        shiny$fluidRow(
          datatable$ui_dt(ns("scoreboardConfiguration"), "Scoreboard")
        )
      })

      # TODO
      scoreboardSheet <-
        readRDS("scoreboardSheet.rda") |>
        dplyr$mutate_if(is.numeric, function(x) round(x, 2)) |>
        dplyr$mutate(
          # analysis, paramcd,
          # Potential.Issue,
          # Potential.Issue.Subfix,
          # flagging_value,
          # Max.Number.Signal.Summary, sitediff.vs.study, Signal.Prefix, Signal.Subfix,
          # Name.of.endpoint.of.interest,
          StudyStatResult = glue$glue(StudyStat),
          SiteStatResult = glue$glue(SiteStat)
          # csm_version,
          # site
        ) |>
        dplyr$mutate(Potential.Issue = sample(c(1, 4, 2), 1)) |>
        dplyr$group_by(Potential.Issue, sitediff.vs.study) |>
        dplyr$select(Potential.Issue, sitediff.vs.study, diff_pct)

      purrr$map_dfr(
        split(scoreboardSheet, scoreboardSheet$Potential.Issue),
        function(x) {
          sort_col <- unique(x$sitediff.vs.study)
          print(sort_col)
          x |>
            dplyr$arrange(sort_col)
        }
      )
      # `# print() |>
      # dplyr$glimpse()
      # dplyr$group_by(Potential.Issue)
      # dplyr$select(sitediff.vs.study)
      # dplyr$group_by(Potential.Issue, Max.Number.Signal.Summary) |>
      # dplyr$count()

      datatable$server_dt("scoreboardConfiguration", data = scoreboardSheet)
    })
  })
}
