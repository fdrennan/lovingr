#' @export
ui_code_review <- function(id = "code_review") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server_code_review <- function(id = "code_review") {
  box::use(shiny, fs, shinyAce, uuid)
  box::use(.. / .. / utilities / io / file_read_multi_ext)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- shiny$renderUI({
        analysis_code_files <- fs$dir_ls("box/analysis/execute", recurse = TRUE, type = "file")
        bs4Dash$box(
          title = "Code Review", width = 12, closable = TRUE, collapsed = TRUE,
          collapsible = TRUE, maximizable = TRUE,
          shiny$fluidRow(
            shiny$column(
              8,
              shiny$wellPanel(
                shiny$selectizeInput(
                  ns("filePathForDisplay"), "File",
                  choices = analysis_code_files, selected = NULL, multiple = TRUE
                )
              )
            ),
            shiny$column(
              4,
              shiny$wellPanel(
                shiny$numericInput(
                  ns("widthOfCols"),
                  label = "Col Width",
                  value = 4, min = 1, max = 12, step = 1
                )
              )
            ),
            shiny$uiOutput(ns("codeUIEditor"), container = function(...) {
              shiny$column(12, shiny$fluidRow(...))
            })
          )
        )
      })

      output$codeUIEditor <- shiny$renderUI({
        shiny$req(input$filePathForDisplay)
        lapply(
          input$filePathForDisplay,
          function(path) {
            shiny$column(
              input$widthOfCols,
              shiny$tags$p(fs$path_file(path)),
              shinyAce$aceEditor(
                outputId = uuid$UUIDgenerate(),
                value = file_read_multi_ext$run(path),
                mode = "r",
                hotkeys = list(
                  helpKey = "F1",
                  runKey = list(
                    win = "Ctrl-R|Ctrl-Shift-Enter",
                    mac = "CMD-ENTER|CMD-SHIFT-ENTER"
                  )
                ),
                wordWrap = TRUE, debounce = 10
              )
            )
          }
        )
      })
    }
  )
}
