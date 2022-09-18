#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = TRUE,
                  width = 12, status = "secondary") {
  box::use(shiny, DT, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$box(
    closable = TRUE,
    maximizable = TRUE,
    width = width,
    status = status,
    solidHeader = TRUE,
    title = title, collapsed = collapsed,
    shiny$fluidRow(
      shiny$uiOutput(ns('filters')),
      shiny$column(12, shiny$downloadButton(ns('download'), 'Download')),
      shiny$column(12, DT$DTOutput(ns("ui"), width = "100%"))
    )
  )
}

#' @export
server_dt <- function(id = "dt", data, pageLength = 3) {
  box::use(shiny, DT, bs4Dash, dplyr, shinyWidgets)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$filters <- shiny$renderUI({
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
      })
      
      output$downloadData <- shiny$downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".rda", sep="")
        },
        content = function(file) {
          saveRDS(data, file)
        }
      )
      
      output$ui <- DT$renderDT(
        server = TRUE,
        {
          shiny$req(input$columnsFilter)
          data <- data[, input$columnsFilter]
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
            selection = "none", #  c("multiple", "single", "none"),
            extensions = list(),
            plugins = NULL,
            editable = FALSE
          )
        }
      )
    }
  )
}
