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
  box::use(.. / .. / utilities / flagging / flag_analysis_data)
  box::use(.. / .. / modules / aei / analysis_aei)
  box::use(.. / .. / modules / rgv / analysis_rgv)
  box::use(.. / .. / modules / rgm / analysis_rgm)
  box::use(.. / .. / modules / missdose / analysis_missdose)
  box::use(.. / .. / modules / diet / analysis_diet)
  box::use(.. / .. / modules / retention / analysis_retention)
  box::use(.. / .. / modules / aecnt / analysis_aecnt)
  box::use(.. / .. / modules / aegap / analysis_aegap)
  box::use(.. / .. / modules / vitals / analysis_vitals)
  box::use(.. / .. / modules / underdose / analysis_underdose)


  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysisOutput <- shiny$reactive({
        browser()
        shiny$req(analysisData)
        analysis_data <- analysisData$data |> dplyr$rename_all(tolower)
        variables <- analysisData$variables |> dplyr$rename_all(tolower)
        results <- switch(analysisData$analysis,
          "aei" = analysis_aei$analysis_aei(analysis_data, variables),
          "rgv" = analysis_rgv$analysis_rgv(analysis_data, variables),
          "aecnt" = analysis_aecnt$analysis_aecnt(analysis_data, variables),
          "aegap" = analysis_aegap$analysis_aegap(analysis_data, variables),
          "vitals" = analysis_vitals$analysis_vitals(analysis_data, variables),
          "underdose" = analysis_underdose$analysis_underdose(analysis_data, variables),
          "rgm" = analysis_rgm$analysis_rgm(analysis_data, variables),
          "diet" = analysis_diet$analysis_diet(analysis_data, variables),
          "missdose" = analysis_missdose$analysis_missdose(analysis_data, variables),
        )
        results <- dplyr$mutate(results, paramcd = tolower(paramcd))
        analysisData$results <- results
        analysisData$flags <-
          flag_analysis_data$flag_analysis_data(results, analysisData$metadata)

        datatable$server_dt("statsResults", results)
        shiny$removeNotification(id = analysisData$analysis)
        analysisData
      })

      shiny$observeEvent(analysisOutput(), {
        output$uiSummary <- shiny$renderUI({
          bs4Dash$box(
            id = ns("analysisResultsBox"),
            collapsed = TRUE, closable = TRUE, maximizable = TRUE,
            width = 12,
            title = "Columns and Flags",
            shiny$fluidRow(
              shiny$column(12, shiny$h1("Inputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names(analysisOutput()$data), function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Outputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names(analysisOutput()$results), function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Flags")),
              shiny$column(12, purrr$map2(
                analysisOutput()$metadata$flagging_value,
                analysisOutput()$metadata$flagging_code,
                function(x, y) {
                  shiny$fluidRow(
                    shiny$column(2, x, class = "d-flex justify-content-center align-items-center"),
                    shiny$column(10, shiny$tags$pre(y), class = "d-flex justify-content-start align-items-center"),
                    shiny$tags$hr()
                  )
                }
              ))
            )
          )
        })

        datatable$server_dt("flags", data = analysisOutput()$flags)
      })

      analysisOutput()
    }
  )
}
