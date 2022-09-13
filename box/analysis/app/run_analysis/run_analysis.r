

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
        class='text-center display-1'
      ),
      shiny$div(
        class='text-right', 
        bs4Dash$actionButton(ns('refresh'), 'Refresh')
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
server_run_analysis <- function(id = "run_analysis", data) {
  box::use(shiny, bs4Dash, shinyAce,readr)
  box::use(.. / .. / .. / utilities / io / file_read_multi_ext)
  
  
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      analysis_name <- unique(data$analysis)
      analysis_code_path <- unique(data$path)
      analysis_file <- file_read_multi_ext$run(analysis_code_path)
      analysis_data <- file_read_multi_ext$run(unique(data$filepath))
      
      output$app <- shiny$renderUI({
        input$reset

        shiny$showNotification('refreshing')
        shiny$column(
          12,
          shiny$fluidRow(
            bs4Dash$box(
              width = 12,
              title = shiny$h1(analysis_name),
              shiny$fluidRow(
                shiny$column( class='text-right',
                  12, bs4Dash$actionButton(ns('commit'), 'Commit')
                ),
                bs4Dash$box(
                  title = paste0('Code Review: ', analysis_name),
                  width = 12,
                  shinyAce$aceEditor(
                    outputId = ns("myEditor"),
                    value = analysis_file,
                    mode = "r",
                    theme = "ambiance"
                  )
                )
              )
            )
          )
        )
      })
      
      
      shiny$observeEvent(input$commit, {
        readr$write_file(input$myEditor[[1]], analysis_code_path)
        
        output$app <- shiny$renderUI({
          shiny$showNotification('refreshing')
          shiny$column(
            12,
            shiny$fluidRow(
              bs4Dash$box(
                width = 12,
                title = shiny$h1(analysis_name),
                shiny$fluidRow(
                  shiny$column( class='text-right',
                                12, bs4Dash$actionButton(ns('commit'), 'Commit')
                  ),
                  bs4Dash$box(
                    title = paste0('Code Review: ', analysis_name),
                    width = 12,
                    shinyAce$aceEditor(
                      outputId = ns("myEditor"),
                      value = file_read_multi_ext$run(analysis_code_path),
                      mode = "r",
                      theme = "ambiance"
                    )
                  )
                )
              )
            )
          )
        })
        
      })
      
    }
  )
}
