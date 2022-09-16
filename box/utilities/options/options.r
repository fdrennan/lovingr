#' @export
ui_options <- function(id = "options", width = 6) {
  box::use(shiny, bs4Dash, shinyToastify)
  ns <- shiny$NS(id)
  bs4Dash$box(
    closable = TRUE,
    maximizable = TRUE,
    collapsed = TRUE, status = "warning",
    title = "Options and General Settings",
    width = width,
    shiny$uiOutput(ns("optionUI"), container = shiny$fluidRow),
    shiny$fluidRow(
      shiny$tags$head(shinyToastify$useShinyToastify()),
      shiny$column(
        12,
        shiny$div(
          class = "text-right py-2",
          bs4Dash$actionButton(ns("resetOptions"), "Reset", status = "warning")
        )
      )
    )
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
            shiny$div(
              class = "col-xl-6 col-lg-6 col-md-12 col-sm-12",
              shiny$fluidRow(
                bs4Dash$bs4Card(
                  title = "General Options", width = 12,
                  shinyWidgets$prettyToggle(
                    ns("chatty"),
                    label_on = "Use Chatty",
                    label_off = "Shut Up",
                    value = FALSE
                  ),
                  shinyWidgets$prettyToggle(
                    ns("ignoreConfigPath"),
                    label_on = "Ignore Configuration Datapaths",
                    label_off = "Use Configuration Datapaths",
                    value = TRUE
                  )
                )
              )
            ),
            shiny$div(
              class = "col-xl-6 col-lg-6 col-md-12 col-sm-12",
              shiny$fluidRow(
                bs4Dash$bs4Card(
                  title = "File Aggregation", width = 12,
                  shiny$textInput(
                    ns("file_regex"),
                    "file_regex",
                    "csm[0-9]{6}[a|b|c]/datamisc$"
                  ),
                  shiny$textInput(
                    ns("base_dir"),
                    "base_dir",
                    paste0(
                      getOption("datamisc_cache_path")
                    )
                  )
                )
              )
            )
          )
        )
      })


      shiny$observeEvent(input$ignoreConfigPath, {
        options(ignoreConfigPath = input$ignoreConfigPath)
        chatty$chatty(session, input)
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
