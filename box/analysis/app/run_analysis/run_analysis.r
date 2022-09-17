#' @export
ui_run_analysis <- function(id = "run_analysis", data) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("app"), container = function(...) {
    shiny$fluidRow(...)
  })
}

#' @export
server_run_analysis <- function(id = "run_analysis", data, variables) {
  box::use(shiny, bs4Dash, shinyAce, readr, dplyr, shinyAce)
  box::use(.. / .. / .. / utilities / chatty / chatty)
  box::use(.. / .. / .. / utilities / io / file_read_multi_ext)
  box::use(.. / .. / .. / utilities / tables / datatable)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysisInput <- shiny$reactive({
        shiny$req(data)
        shiny$req(variables)
        
        analysis_name <- unique(data$analysis)
        analysis_code_path <- unique(data$path)
        analysis_data_path <- unique(data$filepath)
        list(
          analysis_name = analysis_name,
          analysis_code_path = analysis_code_path,
          analysis_file = file_read_multi_ext$run(analysis_code_path),
          analysis_data = {
            analysis_data <- file_read_multi_ext$run(analysis_data_path)[[1]]$data
            names(analysis_data) <- tolower(names(analysis_data))
            if (all(getOption("development"), nrow(analysis_data) > getOption("sample_min"))) {
              analysis_data <- dplyr$sample_frac(analysis_data, getOption("sample_frac"))
            }
            analysis_data
          }
        )
      })

      output$app <- shiny$renderUI({
        shiny$req(analysisInput())
        analysisInput <- analysisInput()
        box::use(purrr)
        bs4Dash$box(
          closable = TRUE,
          status = "success",
          id = ns("analysisBox"),
          width = 12,
          title = shiny$h2("Flagging Results for ", toupper(analysisInput$analysis_name)), collapsed = TRUE,
          shiny$fluidRow(
            shiny$column(
              12,
              shiny$uiOutput(ns("uiSummary"), container = function(...) {
                # shiny$column(12, ...)
                shiny$fluidRow(...)
              }),
              shiny$fluidRow(
                datatable$ui_dt(
                  ns("statsResults"),
                  title = "Pre-Flagging",
                  collapsed = TRUE,
                  width = 12
                ),
                datatable$ui_dt(
                  ns("flags"), "Flags"
                )
              )
            )
          )
        )
      })



      analysisStatistics <- shiny$eventReactive(analysisInput(), {
        box::use(.. / .. / execute / analysis_aei)
        box::use(.. / .. / execute / analysis_rgv)
        box::use(.. / .. / execute / analysis_aecnt)
        box::use(.. / .. / execute / analysis_aegap)
        box::use(.. / .. / execute / analysis_vitals)
        box::use(.. / .. / execute / analysis_underdose)
        box::use(dplyr)
        analysisInput <- analysisInput()

        analysis_name <- analysisInput$analysis_name
        shiny$showNotification(ui = paste0(
          "Generating data for ", analysis_name
        ), id = analysis_name, closeButton = FALSE, duration = NULL)

        analysis_data <- analysisInput$analysis_data
        results <- switch(analysis_name,
          "aei" = analysis_aei$analysis_aei(analysis_data, variables),
          "rgv" = analysis_rgv$analysis_rgv(analysis_data, variables),
          "aecnt" = analysis_aecnt$analysis_aecnt(analysis_data, variables),
          "aegap" = analysis_aegap$analysis_aegap(analysis_data, variables),
          "vitals" = analysis_vitals$analysis_vitals(analysis_data, variables),
          "underdose" = {
            analysis_underdose$analysis_underdose(analysis_data, variables)
          }
        )
        data <- dplyr$select(
          data, study, month, paramcd, flagging_code, flagging_value
        )
        print(analysis_name)
        print(names(results))
        results <- dplyr$mutate(results, paramcd = tolower(paramcd))
        results <- dplyr$inner_join(data, results)
        datatable$server_dt("statsResults", results)
        shiny$removeNotification(id = analysis_name)
        results
      })


      shiny$observeEvent(analysisStatistics(), {
        box::use(dplyr, stats, purrr)
        analysisStatistics <- analysisStatistics()
        analysisInput <- analysisInput()
        analysis_name <- analysisInput$analysis_name
        flaggingSummary <- dplyr$distinct(analysisStatistics, flagging_value, flagging_code)
        flagging_value <- flaggingSummary$flagging_value
        flagging_code <- flaggingSummary$flagging_code

        namesAnalysisStatistics <- names(analysisStatistics)
        doesNotContainName <- stringr::str_detect(
          analysisStatistics$flagging_code,
          namesAnalysisStatistics
        )
        doesNotContainName <- analysisStatistics$flagging_code[doesNotContainName]
        doesNotContainName <- unique(doesNotContainName)
        flags_that_need_fixing <- unique(doesNotContainName)
        corrections_needed <- data.frame(flags_that_need_fixing = flags_that_need_fixing)
        analysis_data <- analysisInput()$analysis_data
        names_statistics_input <- names(analysis_data)
        names_statistics_output <- names(analysisStatistics)


        output$uiSummary <- shiny$renderUI({
          bs4Dash$box(
            collapsed = TRUE, closable = TRUE, maximizable = TRUE,
            width = 12,
            title = "Columns and Flags",
            shiny$fluidRow(
              shiny$column(12, shiny$h1("Inputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names_statistics_input, function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Outputs")),
              shiny$column(12, shiny$fluidRow(
                lapply(names_statistics_output, function(x) {
                  shiny$div(class = "col-xl-2 col-lg-2 col-md-3 col-sm-4 col-xs-4", shiny$h5(x))
                })
              )),
              shiny$column(12, shiny$h1("Flags")),
              shiny$column(12, purrr$map2(flagging_value, flagging_code, function(x, y) {
                shiny$fluidRow(
                  shiny$column(2, x, class = "d-flex justify-content-center align-items-center"),
                  shiny$column(10, shiny$tags$pre(y), class = "d-flex justify-content-start align-items-center"),
                  shiny$tags$hr()
                )
              }))
            )
          )
        })

        tryCatch(
          {
            analysisStatistics <-
              analysisStatistics |>
              dplyr$rowwise() |>
              dplyr$mutate(
                is_flagged = eval(parse(text = flagging_code))
              ) |>
              dplyr$filter(is_flagged) |>
              dplyr$distinct()

            datatable$server_dt("flags", data = analysisStatistics)
          },
          error = function(err) {
            shiny$showNotification(
              closeButton = TRUE, duration = NULL,
              paste0("Flagging failed for ", analysis_name),
            )
          }
        )
      })
    }
  )
}
