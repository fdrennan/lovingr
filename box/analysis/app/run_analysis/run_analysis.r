

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
        bs4Dash$box(
          status = "primary",
          id = ns("analysisBox"),
          width = 12,
          title = shiny$h1(analysisInput$analysis_name), collapsed = FALSE,
          shiny$fluidRow(
            bs4Dash$box(
              maximizable = TRUE,
              title = paste0("Code Review: ", analysisInput$analysis_name),
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
              title = "Stats Results",
              collapsed = TRUE, width = 12
            )
          )
        )
      })



      shiny$observeEvent(analysisInput(), {
        box::use(.. / .. / execute / analysis_aei)
        box::use(.. / .. / execute / analysis_rgv)
        box::use(.. / .. / execute / analysis_aecnt)
        box::use(.. / .. / execute / analysis_aegap)
        analysisInput <- analysisInput()

        analysis_name <- analysisInput$analysis_name
        shiny$showNotification(ui = paste0(
          "Generating data for ", analysis_name
        ), id = analysis_name, closeButton = FALSE, duration = NULL)

        results <- switch(analysis_name,
          "aei" = analysis_aei$analysis_aei(analysisInput$analysis_data, variables),
          "rgv" = analysis_rgv$analysis_rgv(analysisInput$analysis_data, variables),
          "aecnt" = analysis_aecnt$analysis_aecnt(analysisInput$analysis_data, variables),
          "aegap" = analysis_aegap$analysis_aegap(analysisInput$analysis_data, variables)
        )
        datatable$server_dt("statsResults", results)
        shiny$removeNotification(id = analysis_name)
        # bs4Dash$updateBox(id = "#analysisBox", action = "update", options = list(
        #   closable = TRUE, status = "primary"
        # ))
        # bs4Dash$updateBox(id = ns("analysisBox"), action = "update", options = list(
        #   closable = TRUE, status = "primary"
        # ))
        # bs4Dash$updateBox(id = paste0("#", ns("analysisBox")), action = "update", options = list(
        #   closable = TRUE, status = "primary"
        # ))
      })
    }
  )
}
