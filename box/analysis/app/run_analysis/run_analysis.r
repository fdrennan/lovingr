#' @export
ui_run_analysis <- function(id = "run_analysis", data) {
  box::use(shiny, bs4Dash, shinyWidgets)
  ns <- shiny$NS(id)
  box::use(.. / .. / .. / utilities / tables / datatable)
  shiny$fluidRow(
    class = "border-top my-3",
    shiny$column(12, shiny$h3(shiny$h4(toupper(data$analysis)), class = "display-4")),
    shiny$column(12,
      class = "d-flex justify-content-between align-items-top",
      shinyWidgets$switchInput(ns("runWithDebugger"), size = "mini", inline = TRUE, "Run With Debugger", value = FALSE),
      bs4Dash$actionButton(ns("runAgain"), "Run Again", size = "xs")
    ),
    shiny$column(
      12,
      shiny$uiOutput(ns("ui"), container = function(...) {
        shiny$fluidRow(...)
      })
    ),
    shiny$tags$hr()
  )
}

#' @export
server_run_analysis <- function(id = "run_analysis", preAnalysisData) {
  {
    box::use(shiny, bs4Dash, shinyAce, readr, glue, dplyr, stats, shinyAce, purrr)
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
  }


  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysis_data <- preAnalysisData$data |> dplyr$rename_all(tolower)
      variables <- preAnalysisData$variables |> dplyr$rename_all(tolower)

      shouldDebug <- shiny$reactive({
        message("shouldDebug")
        input$runWithDebugger
      })

      postAnalysisData <- shiny$reactive({
        message("postAnalysisData")
        print(input$runAgain)
        shiny$req(preAnalysisData)
        shiny$req(is.logical(input$runWithDebugger))

        if (shouldDebug()) {
          print(preAnalysisData)
          do.call("browser", list())
        }
        results <- tryCatch(
          {
            results <- switch(preAnalysisData$analysis,
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

            if (shouldDebug()) {
              print(results)
              do.call("browser", list())
            }

            results <- dplyr$mutate(results, paramcd = tolower(paramcd))
            postAnalysisData <- list()
            postAnalysisData <- append(preAnalysisData, results)
            postAnalysisData$flags <- flag_analysis_data$flag_analysis_data(results, postAnalysisData$metadata)

            datatable$server_dt("statsResults", results)
            shiny$removeNotification(id = postAnalysisData$analysis)
            postAnalysisData$status <- "success"
            postAnalysisData$title <- {
              paste0("Flagging Results for ", toupper(postAnalysisData$analysis))
            }
            postAnalysisData$footer <- shiny$fluidRow(
              shiny$column(
                12, shiny$h5(glue$glue("Flags: {nrow(results)}"))
              )
            )
            postAnalysisData$body <-
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

            postAnalysisData
          },
          error = function(err) {
            if (shouldDebug()) do.call("browser", list())
            postAnalysisData <- list()
            postAnalysisData$err <- err
            postAnalysisData$title <- shiny$fluidRow(
              shiny$column(12, glue$glue("Failure in {preAnalysisData$analysis}"))
            )
            postAnalysisData$status <- "danger"
            postAnalysisData$body <-
              shiny$fluidRow(
                shiny$column(
                  12,
                  shiny$tags$h4("Call"),
                  shiny$tags$pre(paste0(deparse(err$call), collapse = "\n")),
                  shiny$tags$h4("Message"),
                  shiny$tags$code(paste0(err$message, collapse = "\n"))
                )
              )
            postAnalysisData
          }
        )

        results$ns <- ns
        results
      })



      shiny$observeEvent(postAnalysisData(), {
        message("ui")
        output$ui <- shiny$renderUI({
          bs4Dash$box(
            postAnalysisData()$body,
            id = ns("analysisBox"),
            title = postAnalysisData()$title,
            width = 12,
            status = postAnalysisData()$status,
            collapsible = TRUE,
            maximizable = TRUE,
            closable = TRUE,
            collapsed = ifelse(postAnalysisData()$status == "danger", TRUE, FALSE),
            footer = postAnalysisData()$footer
          )
        })
      })


      shiny$observeEvent(postAnalysisData(), {
        message("uiSummary")
        output$uiSummary <- shiny$renderUI({
          bs4Dash$box(
            id = ns("analysisResultsBox"),
            collapsed = TRUE, closable = TRUE, maximizable = TRUE,
            width = 12,
            title = "Columns and Flags",
            shiny$fluidRow(
              shiny$column(12, shiny$h1("Inputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names(postAnalysisData()$data), function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Outputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names(postAnalysisData()$results), function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Flags")),
              shiny$column(12, purrr$map2(
                postAnalysisData()$metadata$flagging_value,
                postAnalysisData()$metadata$flagging_code,
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

        datatable$server_dt("flags", data = postAnalysisData()$flags)
      })

      postAnalysisData
    }
  )
}
