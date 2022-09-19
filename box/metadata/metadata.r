#' @export
ui_metadata <- function(id = "metadata", width = 6) {
  box::use(shiny, bs4Dash, shinyFiles)
  box::use(.. / utilities / io / file_upload)
  ns <- shiny$NS(id)
  bs4Dash$box(
    title = "Data Manager", closable = TRUE, id = ns("dataImport"), status = "primary", maximizable = TRUE, width = 12,
    shiny$fluidRow(
      shiny$column(
        12,
        shiny$p(
          shiny$h3("1. Import CSM Data from Filesystem"),
          shinyFiles$shinyDirButton(ns("inputDir"),
            "Import",
            "Please select a folder to import CSM data from.", FALSE,
            class = "btn btn-lg btn-primary"
          )
        )
      ),
      shiny$column(12, id = "metadatawarning", shiny$tags$p(
        "Selectors for study, year, month, and analysis type will be generated after choosing a root directory for search."
      ), shiny$tags$p(
        "Candidate files for analysis are set in the .Rprofile"
      )),
      shiny$uiOutput(ns("study"), container = function(...) {
        shiny$column(6, ...)
      }),
      shiny$uiOutput(ns("year"), container = function(...) {
        shiny$column(6, ...)
      }),
      shiny$uiOutput(ns("month"), container = function(...) {
        shiny$column(6, ...)
      }),
      shiny$uiOutput(ns("analysis"), container = function(...) {
        shiny$column(6, ...)
      }),
      shiny$uiOutput(ns("configurationUploadPanel"), container = function(...) {
        shiny$column(12, ...)
      })
    )
  )
}


#' @export
server_metadata <- function(id = "metadata") {
  box::use(.. / utilities / read / xlsx)
  box::use(shiny, dplyr, stats, bs4Dash, fs, shinyFiles, openxlsx)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / csm_config / clean)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

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
        shiny$removeUI("#metadatawarning")
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

        shiny$selectizeInput(ns("analysis"), shiny$h5("Analysis"),
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


      output$configurationUploadPanel <- shiny$renderUI({
        shiny$req(filteredData())
        shiny$fluidRow(
          shiny$column(
            12,
            shiny$p(
              shiny$h3("2. Download and Set up Configuration."),
              shiny$downloadButton(ns("downloadData"), "Download Configuration Template")
            )
          ),
          shiny$column(
            12,
            shiny$p(
              shiny$h3("3. Upload a configuration file."),
              shiny$fileInput(
                inputId = ns("fileUpload"),
                label = "",
                accept = "*",
                multiple = TRUE
              )
            )
          )
        )
      })

      output$downloadData <- shiny$downloadHandler(
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        filename = "Config.xlsx",
        content = function(file) {
          file.copy("www/Config.xlsx", file)
        }
      )

      config <- shiny$reactive({
        shiny$req(input$fileUpload)
        shiny$req(filteredData())
        # debug(xlsx$server_xlsx)
        # browser()
        configPath <- input$fileUpload$datapath
        sheetNames <- openxlsx$getSheetNames(configPath)
        out <- lapply(sheetNames, function(sheetName) {
          list(
            sheetName = sheetName,
            data = openxlsx$read.xlsx(configPath, sheetName)
          )
        })
        # browser()
        out <- dplyr$inner_join(clean$clean_config(out), filteredData())
        out
      })

      config
    }
  )
}
