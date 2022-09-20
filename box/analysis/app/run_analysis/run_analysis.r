#' @export
ui_run_analysis <- function(id = "run_analysis", data) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  box::use(.. / .. / .. / utilities / tables / datatable)
  bs4Dash$box(
    collapsible = TRUE, maximizable = TRUE,
    closable = TRUE,
    id = ns("analysisBox"),
    width = 12,
    title = paste0("Flagging Results for ", toupper(unique(data$analysis))), collapsed = TRUE,
    shiny$fluidRow(
      shiny$column(
        12,
        shiny$uiOutput(ns("uiSummary"), container = function(...) {
          shiny$fluidRow(...)
        }),
        shiny$fluidRow(
          datatable$ui_dt(
            ns("statsResults"),
            title = "Pre-Flagging",
            width = 12
          ),
          datatable$ui_dt(
            ns("flags"), "Flags"
          )
        )
      )
    )
  )
}

#' @export
server_run_analysis <- function(id = "run_analysis", analysisData) {
  box::use(shiny, bs4Dash, shinyAce, readr, dplyr, stats, shinyAce, purrr)
  box::use(.. / .. / .. / utilities / tables / datatable)
  box::use(.. / .. / modules / aei / analysis_aei)
  box::use(.. / .. / modules / rgv / analysis_rgv)
  box::use(.. / .. / modules / aecnt / analysis_aecnt)
  box::use(.. / .. / modules / aegap / analysis_aegap)
  box::use(.. / .. / modules / vitals / analysis_vitals)
  box::use(.. / .. / modules / underdose / analysis_underdose)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysisOutput <- shiny$reactive({
        shiny$req(analysisData)
        analysis_data <- analysisData$data |> dplyr$rename_all(tolower)
        variables <- analysisData$variables
        results <- switch(analysisData$analysis,
          "aei" = analysis_aei$analysis_aei(analysis_data, variables),
          "rgv" = analysis_rgv$analysis_rgv(analysis_data, variables),
          "aecnt" = analysis_aecnt$analysis_aecnt(analysis_data, variables),
          "aegap" = analysis_aegap$analysis_aegap(analysis_data, variables),
          "vitals" = analysis_vitals$analysis_vitals(analysis_data, variables),
          "underdose" = analysis_underdose$analysis_underdose(analysis_data, variables)
        )
        results <- dplyr$mutate(results, paramcd = tolower(paramcd))
        analysisData$results <- results
        datatable$server_dt("statsResults", results)
        shiny$removeNotification(id = analysisData$analysis)
        results
      })

      analysisSummary <- shiny$eventReactive(analysisOutput(), {
        analysisOutput <- analysisOutput()
        analysisDataPrep <- analysisDataPrep()
        browser()
        analysis_name <- analysisDataPrep$analysis_name
        flaggingSummary <- dplyr$distinct(analysisOutput, flagging_value, flagging_code)
        flagging_value <- flaggingSummary$flagging_value
        flagging_code <- flaggingSummary$flagging_code

        analysis_data <- analysisDataPrep()$analysis_data
        names_statistics_input <- names(analysis_data)
        names_statistics_output <- names(analysisOutput)
        analysisOutput <-
          analysisOutput |>
          dplyr$rowwise() |>
          dplyr$mutate(
            is_flagged = eval(parse(text = flagging_code))
          ) |>
          dplyr$filter(is_flagged) |>
          dplyr$distinct()
        list(
          analysis_input = analysisDataPrep()$analysis_data,
          analysisOutput = analysisOutput,
          names_statistics_input = names_statistics_input,
          names_statistics_output = names_statistics_output,
          flagging_value = flagging_value,
          flagging_code = flagging_code
        )
      })

      shiny$observeEvent(analysisSummary(), {
        box::use(purrr)
        analysisSummary <- analysisSummary()
        output$uiSummary <- shiny$renderUI({
          bs4Dash$box(
            collapsed = TRUE, closable = TRUE, maximizable = TRUE,
            width = 12,
            title = "Columns and Flags",
            shiny$fluidRow(
              shiny$column(12, shiny$h1("Inputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(analysisSummary$names_statistics_input, function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Outputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(analysisSummary$names_statistics_output, function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Flags")),
              shiny$column(12, purrr$map2(analysisSummary$flagging_value, analysisSummary$flagging_code, function(x, y) {
                shiny$fluidRow(
                  shiny$column(2, x, class = "d-flex justify-content-center align-items-center"),
                  shiny$column(10, shiny$tags$pre(y), class = "d-flex justify-content-start align-items-center"),
                  shiny$tags$hr()
                )
              }))
            )
          )
        })

        datatable$server_dt("flags", data = analysisSummary$analysisOutput)
      })

      # analysisSummaryToScoreboard <- shiny$eventReactive(analysisSummary(), {
      #   box::use(dplyr, stats, purrr)
      #   analysisSummary <- analysisSummary()
      #   analysisSummary
      # })

      analysisSummary()
    }
  )
}
