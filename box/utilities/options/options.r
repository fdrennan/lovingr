#' @export
ui_options <- function(id = "options", width = 6) {
  box::use(shiny, bs4Dash, shinyToastify)
  ns <- shiny$NS(id)
  bs4Dash$box(
    title = "Options and General Settings",
    width = width,
    shiny$fluidRow(
      shiny$tags$head(shinyToastify$useShinyToastify()),
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
  box::use(.. / chatty / chatty)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      options(chatty = FALSE)

      output$optionUI <- shiny$renderUI({
        input$resetOptions
        shiny$column(
          12,
          shiny$fluidRow(
            bs4Dash$bs4Card(
              title = "General Options", width = 12,
              shinyWidgets$prettyToggle(
                ns("chatty"),
                label_on = "Use Chatty",
                label_off = "Shut Up",
                value = FALSE
              )
            ),
            bs4Dash$bs4Card(
              title = "File Aggregation", width = 12,
              shiny$textInput(ns("file_regex"), "file_regex", "csm[0-9]{6}[a|b|c]/datamisc$"),
              shiny$textInput(
                ns("base_dir"),
                "base_dir",
                paste0(
                  getOption("datamisc_cache_path"),
                  getOption("bmrn_base_dir")
                )
              )
            )
          )
        )
      })


      shiny$observeEvent(input$chatty, {
        options(chatty = input$chatty)
        chatty$chatty(session, input)
      })

      shiny$observeEvent(input$file_regex, {
        options(file_regex = input$file_regex)
        chatty$chatty(session, input)
      })


      shiny$observeEvent(input$base_dir, {
        options(base_dir = input$base_dir)
        chatty$chatty(session, input)
      })


      input
    }
  )
}
