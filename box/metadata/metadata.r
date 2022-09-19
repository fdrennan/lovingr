#' @export
ui_metadata <- function(id = "metadata", width = 6) {
  box::use(shiny, bs4Dash, shinyFiles)
  ns <- shiny$NS(id)
  input_container <- function(...) {
    shiny$column(6, ...)
  }

  bs4Dash$box(
    title = "File Aggregation", width = 6,
    shiny$fluidRow(
      shiny$column(
        12,
        shiny$wellPanel(
          shinyFiles$shinyDirButton(ns("inputDir"),
            "Input Directory",
            "Please select a folder", FALSE,
            class = "btn btn-primary"
          ),
          shiny$uiOutput(ns("study"), container = input_container),
          shiny$uiOutput(ns("year"), container = input_container),
          shiny$uiOutput(ns("month"), container = input_container),
          shiny$uiOutput(ns("analysis"), container = input_container),
          shiny$downloadButton(ns("downloadData"), "Download Example")
        )
      )
    )
  )
}


#' @export
server_metadata <- function(id = "metadata") {
  box::use(shiny, dplyr, stats, bs4Dash, fs, shinyFiles)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$downloadData <- shiny$downloadHandler(
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        filename = function() {
          "Config.xlsx"
        },
        content = function(file) {
          file.copy("www/Config.xlsx", file)
        }
      )

      shinyFiles$shinyDirChoose(input, id = "inputDir", roots = c(`Working Directory` = getwd(), Root = "/"))

      datafiles <- shiny$eventReactive(input$inputDir, {
        shiny$req(!inherits(input$inputDir, "shinyActionButtonValue"))

        base_directory <- input$inputDir$path[[2]]
        box::use(.. / cdm / meta)

        datafiles <- meta$get_data(base_directory)
        datafiles
      })

      output$study <- shiny$renderUI({
        shiny$req(datafiles())

        datafiles <- datafiles()
        study <- datafiles$study
        shiny$selectizeInput(ns("study"), shiny$h5("Study"),
          choices = unique(study),
          selected = unique(study)[[1]],
          multiple = FALSE
        )
      })

      output$year <- shiny$renderUI({
        shiny$req(input$study)

        datafiles <- datafiles()
        year <- datafiles |>
          dplyr$filter(study %in% input$study) |>
          dplyr$pull(year)

        shiny$selectizeInput(
          ns("year"),
          shiny$h5("Year"),
          choices = unique(year),
          selected = max(as.numeric(year)),
          multiple = FALSE
        )
      })

      output$month <- shiny$renderUI({
        shiny$req(input$year)
        datafiles <- dplyr$filter(datafiles(), study %in% input$study, year %in% input$year)
        monthName <- dplyr$pull(datafiles, monthName)
        max_month <- datafiles |>
          dplyr$filter(date == max(date)) |>
          dplyr$pull(monthName)

        shiny$selectizeInput(
          ns("monthName"),
          shiny$h5("Month"),
          choices = unique(monthName),
          selected = unique(max_month),
          multiple = TRUE
        )
      })

      output$analysis <- shiny$renderUI({
        shiny$req(input$monthName)
        shiny$req(datafiles())
        datafiles <- datafiles()

        analysis <- datafiles |>
          dplyr$filter(
            study %in% input$study,
            year %in% input$year,
            monthName %in% input$monthName
          ) |>
          dplyr$pull(analysis)


        shiny$selectizeInput(ns("analysis"), "Analysis",
          choices = analysis,
          selected = analysis, multiple = TRUE
        )
      })

      filteredData <- shiny$eventReactive(
        input$analysis,
        {
          datafiles <- datafiles()
          files <- datafiles |>
            dplyr$filter(
              study %in% input$study,
              year %in% input$year,
              monthName %in% input$monthName,
              analysis %in% input$analysis
            ) |>
            dplyr$select(
              study, month, monthName, year, date,
              analysis, filename,
              filepath = path
            )

          files
        }
      )

      filteredData
    }
  )
}
