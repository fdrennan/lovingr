

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
  box::use(shiny, bs4Dash, shinyAce, readr)
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
            analysis_data
          }
        )
      })

      output$app <- shiny$renderUI({
        shiny$req(analysisInput())
        analysisInput <- analysisInput()
        bs4Dash$box(closable=TRUE,
          status = "success",
          id = ns("analysisBox"),
          width = 12,
          title = shiny$h1(analysisInput$analysis_name), collapsed = TRUE,
          shiny$fluidRow(
            bs4Dash$box(closable=TRUE,
              maximizable = TRUE,
              title = "Code Review", status = "info",
              width = 12, collapsed = TRUE,
              shinyAce$aceEditor(
                outputId = ns("myEditor"),
                value = analysisInput$analysis_file,
                mode = "r",
                theme = "ambiance"
              )
            ),
            datatable$ui_dt(
              ns("statsResults"),
              title = "Pre-Flagging",
              collapsed = TRUE, 
              width = 12
            )
          ),
          shiny$uiOutput(ns('uiSummary'), container = function(...) {
            shiny$fluidRow(shiny$column(12, ...))
          })
        )
      })



      analysisStatistics <- shiny$eventReactive(analysisInput(), {
        #
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
          data, study, month, paramcd, flagging_code
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
        analysisStatistics <- analysisStatistics()
        namesAnalysisStatistics <- names(analysisStatistics)
        doesNotContainName <- stringr::str_detect(analysisStatistics$flagging_code, namesAnalysisStatistics)
        doesNotContainName <- analysisStatistics$flagging_code[doesNotContainName]
        doesNotContainName <- unique(doesNotContainName)
        flags_that_need_fixing = unique(doesNotContainName)
        corrections_needed <- data.frame(flags_that_need_fixing = flags_that_need_fixing)
        analysis_data <- analysisInput()$analysis_data
        names_statistics_input <- names(analysis_data)
        names_statistics_output <- names(analysisStatistics)
        
        output$correctionsOutput <- shiny$renderTable({
          corrections_needed
        })
        output$uiSummary <- shiny$renderUI({
          shiny$fluidRow(
            shiny$column(12, shiny$h1('Inputs')),
            lapply(names_statistics_input, function(x) {
              shiny$column(2, shiny$h4(x))
            }),
            shiny$column(12, shiny$h1('Outputs')),
            lapply(names_statistics_output, function(x) {
              shiny$column(2, shiny$h4(x))
            }),
            shiny$column(12, shiny$h1('Corrections Needed')),
            shiny$tableOutput(ns('correctionsOutput'))
          )
        })
        # datatable$server_dt('namesTable', corrections_needed, title = 'Potential Issues')
      })
    }
  )
}
