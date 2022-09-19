#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = FALSE,
                  width = 12, status = "secondary") {
  box::use(shiny, DT, bs4Dash, esquisse)
  ns <- shiny$NS(id)
  print(ns("testing-ui"))
  bs4Dash$box(closable = TRUE, maximizable = TRUE, width = width, status = status,
    solidHeader = TRUE,
    title = title, collapsed = collapsed,
    shiny$fluidRow(
      shiny$uiOutput(ns("filters"), container = function(...) {
        shiny$column(12, ...)
      }),
      shiny$column(12, shiny$downloadButton(ns("downloadData"), "Download")),
      bs4Dash$box(
        title = "Table", collapsible = TRUE, collapsed = FALSE, width = 12,
        DT$DTOutput(ns("ui"), width = "100%")
      )
      # bs4Dash$box(
      #   title = "Plotting", collapsible = TRUE, width = 12, collapsed = FALSE, header = FALSE,
      #   esquisse$esquisse_ui(paste0('esquisse', title))
      # )
    )
  )
}

#' @export
server_dt <- function(id = "dt", data, title, pageLength = 3) {
  box::use(shiny, DT, esquisse, bs4Dash, dplyr, shinyWidgets, readr, writexl)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      output$filters <- shiny$renderUI({
        # 
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
          filename = "output.xlsx",
          content = function(file) {
            writexl$write_xlsx(cleanedData(), file)
          }
        )

      cleanedData <- shiny$reactive({
        if (!is.null(input$columnsFilter)) {
          data <- data[, input$columnsFilter]
        }

        output$ui <- DT$renderDT(
          server = TRUE,
          {
            DT::datatable(data,
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


        data
      })

      # data_rv <- shiny$reactiveValues(data = cleanedData(), name = ns("data"))
      # esquisse$esquisse_server(paste0('esquisse', title), data_rv)

      cleanedData()
    }
  )
}
