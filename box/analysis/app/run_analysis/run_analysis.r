

#' @export
ui_run_analysis <- function(id = "run_analysis", data) {
  box::use(shiny, bs4Dash)
  # box::use(../../../utilities/io/file_read_multi_ext)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(
      12,
      shiny$h1(
        unique(data$analysis),
        class = "text-center display-1"
      ),
      shiny$div(
        class = "text-right",
        bs4Dash$actionButton(ns("refresh"), "Refresh")
      )
    ),
    shiny$column(
      12,
      shiny$uiOutput(ns("app"), container = function(...) {
        shiny$fluidRow(...)
      })
    )
  )
}



#' @export
server_run_analysis <- function(id = "run_analysis", data, variables) {
  box::use(shiny, bs4Dash, shinyAce, readr)
  box::use(.. / .. / .. / utilities / io / file_read_multi_ext)
  box::use(.. / .. / .. / utilities / tables / datatable)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysis_name <- unique(data$analysis)
      analysis_code_path <- unique(data$path)
      analysis_file <- file_read_multi_ext$run(analysis_code_path)
      analysis_data <- file_read_multi_ext$run(unique(data$filepath))[[1]]$data
      names(analysis_data) <- tolower(names(analysis_data))

      output$app <- shiny$renderUI({
        input$reset
        shiny$column(
          12,
          shiny$fluidRow(
            bs4Dash$box(
              width = 12,
              title = shiny$h1(analysis_name), collapsed = FALSE,
              shiny$fluidRow(
                bs4Dash$box(
                  title = paste0("Code Review: ", analysis_name),
                  width = 12, collapsed = TRUE,
                  shinyAce$aceEditor(
                    outputId = ns("myEditor"),
                    value = analysis_file,
                    mode = "r",
                    theme = "ambiance"
                  )
                ),
                shiny$column(
                  12, shiny$div(
                    class = "text-center py-3",
                    bs4Dash$actionButton(ns("execute"), "Execute")
                  )
                ),
                datatable$ui_dt(
                  ns("statsResults"),
                  title = "Stats Results",
                  collapsed = FALSE, width = 12
                )
              )
            )
          )
        )
      })

      shiny$observeEvent(input$execute, {
        box::use(.. / .. / execute / analysis_aei)
        results <- analysis_aei$analysis_aei(analysis_data, variables)
        datatable$server_dt("statsResults", results)
      })
    }
  )
}
