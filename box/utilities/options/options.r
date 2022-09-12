#' @export
ui_options <- function(id = "options", width = 6) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    title = "Break Things",
    width = width,
    shiny$fluidRow(
      shiny$column(
        12,
        shiny$div(
          class = "text-right py-2",
          bs4Dash$actionButton(ns("resetOptions"), "Reset")
        )
      )
    ),
    shiny$uiOutput(ns("optionUI"), container = shiny$fluidRow)
  )
}

#' @export
server_options <- function(id = "options") {
  box::use(shiny, glue, shinyWidgets, bs4Dash)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$optionUI <- shiny$renderUI({
        input$resetOptions
        shiny$column(
          12,
          shiny$fluidRow(
            bs4Dash$bs4Card(
              title = "General Options", width = 12,
              shinyWidgets$prettyToggle(
                ns("development"),
                label_on = "Development",
                label_off = "Production",
                value = TRUE
              )
            ),
            bs4Dash$bs4Card(
              title = "File Aggregation", width = 12,
              shiny$textInput(ns("file_regex"), "file_regex", "csm[0-9]{6}[a|b|c]/datamisc$"),
              shiny$textInput(ns("bmrn_base_dir"), "bmrn_base_dir", "/sassys/cdm/cdmdev/bmn111/ach"),
              shiny$textInput(ns("local_base_prefix"), "local_base_prefix", "datamisc")
            )
          )
        )
      })


      shiny$observeEvent(input$development, {
        options(development = input$development)
      })

      shiny$observeEvent(input$file_regex, {
        options(file_regex = input$file_regex)
      })

      shiny$observeEvent(input$local_base_prefix, {
        options(local_base_prefix = input$local_base_prefix)
      })
    }
  )
}
