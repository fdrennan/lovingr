

#' @export
ui_body <- function(id = "body") {
  # Imports
  {
    box::use(shiny, bs4Dash, shinyFiles, fs)
    box::use(.. / utilities / options / options)
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / tables / datatable)
    box::use(.. / utilities / codereview / codereview)
    box::use(.. / metadata / metadata)
  }

  box::use(sortable)

  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    shiny$includeCSS("www/styles.css"),
    shiny$fluidRow(
      codereview$ui_code_review(),
      shiny$column(8,
        offset = 2,
        metadata$ui_metadata(ns("metadata"), width = 12),
        shiny$uiOutput(ns("dataRaw"), container = function(...) {
          shiny$column(12, ...)
        }),
        shiny$uiOutput(ns("codeUI"), container = function(...) {
          shiny$column(12, ...)
        }),
        shiny$uiOutput(ns("scoreboard"), container = function(...) {
          shiny$column(12, ...)
        })
      )
    )
  )
}


#' @export
server_body <- function(id = "body", appSession) {
  # Imports
  {
    box::use(shiny, uuid, bs4Dash, dplyr, shinyFiles, fs, utils, purrr, shinyAce, jsonlite)
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
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      codereview$server_code_review()
      metadata <- metadata$server_metadata("metadata")

      shiny$observeEvent(metadata(), {
        output$dataRaw <- shiny$renderUI({
          shiny$req(metadata())
          shiny$fluidRow(
            shiny$column(12, id = "dataPreview"),
            bs4Dash$box(
              width = 12,
              title = "Flagging Results and Review",
              shiny$div(id = "uiAnalyses")
            ),
            shiny$column(
              class = "d-flex justify-content-end align-items-center p-2", 12,
              bs4Dash$actionButton(ns("getResults"), "Get Results")
            )
          )
        })
      })

      shiny$observeEvent(metadata(), {
        metadata <- metadata()
        import_files <- dplyr$distinct(metadata()$clean, analysis, filepath)
        uuid <- uuid::UUIDgenerate()
        shiny$removeUI("#dataPreviewElements")
        shiny$insertUI(
          "#dataPreview", "afterBegin",
          shiny$column(12,
            id = "dataPreviewElements"
          )
        )

        output <- purrr$map(
          import_files$filepath,
          function(path) {
            shiny$insertUI("#dataPreviewElements", "afterBegin", xlsx$ui_xlsx(ns(uuid)))
            out <- xlsx$server_xlsx(uuid, datapath = path, ui_id = "#dataPreviewElements")
          }
        )
        #
        output
      })

      dataForScoreboard <- shiny$eventReactive(metadata(), {
        clean_metadata <- metadata()$clean
        raw_metadata <- metadata()$raw
        clean_metadata <- split(clean_metadata, clean_metadata$analysis)
        n_increments <- length(clean_metadata)

        output <-
          purrr$imap(
            clean_metadata,
            function(analysis_data, name) {
              shiny$insertUI(
                "#uiAnalyses",
                "afterBegin",
                run_analysis$ui_run_analysis(
                  ns(paste0("run_analysis", name)), analysis_data
                )
              )
              variables <- dplyr$mutate_all(raw_metadata[[1]]$data, tolower)
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
        dataForScoreboard(),
        {
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
              out <- data$analysisStatistics
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
          datatable$server_dt(
            "scoreboardConfiguration",
            data = scoreboardSheet
          )
        }
      )
    }
  )
}
