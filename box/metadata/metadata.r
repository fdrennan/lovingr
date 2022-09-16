#' @export
ui_metadata <- function(id = "metadata", width = 6) {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  input_container <- function(...) {
    shiny$column(6, ...)
  }
  bs4Dash$box(
    closable = TRUE,
    title = "Select Study Information",
    width = width,
    status = "secondary",
    shiny$fluidRow(
      shiny$uiOutput(ns("study"), container = input_container),
      shiny$uiOutput(ns("year"), container = input_container),
      shiny$uiOutput(ns("month"), container = input_container),
      shiny$uiOutput(ns("analysis"), container = input_container)
    )
  )
}


#' @export
server_metadata <- function(id = "metadata") {
  box::use(shiny, dplyr, stats, bs4Dash, fs)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      datafiles <- shiny$reactive({
        box::use(.. / caching / cache)
        datafiles <- cache$check()
        datafiles
      })

      output$study <- shiny$renderUI({
        shiny$req(datafiles())
        datafiles <- datafiles()
        study <- datafiles$study
        shiny$selectizeInput(ns("study"), shiny$h5("Study"),
          choices = unique(study),
          selected = "111302",
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
        datafiles <- datafiles()

        analysis <- datafiles |>
          dplyr$filter(
            study %in% input$study,
            year %in% input$year,
            monthName %in% input$monthName
          ) |>
          dplyr$pull(analysis)


        if (!is.null(getOption("analysis_filter"))) analysis <- getOption("analysis_filter")
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

          if (nchar(getOption("datamisc_cache_path")) > 0) {
            files$filepath <- paste0(getOption("datamisc_cache_path"), files$filepath)
          }

          files
        }
      )

      filteredData
    }
  )
}
