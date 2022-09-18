#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = TRUE,
                  width = 12, status = "secondary", esquisse_it=TRUE) {
  box::use(shiny, DT, bs4Dash, esquisse)
  ns <- shiny$NS(id)
  bs4Dash$box(
    closable = TRUE,
    maximizable = TRUE,
    width = width,
    status = status,
    solidHeader = TRUE,
    title = title, collapsed = collapsed,
    shiny$fluidRow(
      shiny$uiOutput(ns("filters"), container = function(...) {
        shiny$column(12, ...)
      }),
      shiny$column(12, shiny$downloadButton(ns("downloadData"), "Download")),
      shiny$column(12, DT$DTOutput(ns("ui"), width = "100%")),
      shiny$column(
        12, 
        {
          if(esquisse_it) {
            print('ui')
            print(ns('esquisse'))
            esquisse$esquisse_ui(ns('esquisse'), header = FALSE)
          } else {
            shiny$div()
          }
        }
      )
    )
  )
}

#' @export
server_dt <- function(id = "dt", data, pageLength = 3, esquisse_it=TRUE) {
  box::use(shiny, DT, esquisse, bs4Dash, dplyr, shinyWidgets, readr, writexl)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      if (esquisse_it) {
        print('server')
        data_rv <- shiny$reactiveValues(data = data, name = ns("esquissedata"))
        print(ns('esquisse'))
        esquisse$esquisse_server('esquisse',data_rv) 
      }
      
      output$filters <- shiny$renderUI({
        shiny$fluidRow(
          shiny$column(
            12,
            shinyWidgets$pickerInput(
              inputId = ns("columnsFilter"),
              label = "Select Columns",
              choices = names(data),
              selected = names(data),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          )
        )
      })

      output$downloadData <-
        shiny$downloadHandler(
          contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          filename = 'Config.xlsx',
          content = function(file) {
            writexl$write_xlsx(cleanedData(), file)
          }
        )
      
      cleanedData <- shiny$reactive({
        shiny$req(input$columnsFilter)
        data <- data[, input$columnsFilter]
      })
      
      # shiny$observeEvent(cleanedData(), {
      #   browser()
      #   data_rv <- shiny$reactiveValues(data = cleanedData(), name = ns('data'))
      #   esquisse$esquisse_server('esquisse',data_rv)
      # })
      
      # browser()
      output$ui <- DT$renderDT(
        server = TRUE,
        {
          # shiny$req(cleanedData())
          DT::datatable(cleanedData(),
                        options = list(
                          scrollX = TRUE,
                          pageLength = pageLength,
                          filter = "top"
                        ),
                        class = "compact",
                        caption = NULL,
                        filter = c("top"),
                        escape = TRUE,
                        style = "bootstrap4",
                        width = NULL,
                        height = NULL,
                        elementId = NULL,
                        fillContainer = getOption("DT.fillContainer", NULL),
                        autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
                        selection = "multiple", #  c("multiple", "single", "none"),
                        extensions = list(),
                        plugins = NULL,
                        editable = FALSE
          )
        }
      )
      
    }
  )
}
