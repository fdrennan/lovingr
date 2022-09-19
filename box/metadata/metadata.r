#' @export
ui_metadata <- function(id = "metadata", width = 6) {
  box::use(shiny, bs4Dash, shinyFiles)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / tables / datatable)
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
      ), shiny$tags$p(
        "Analysis Metadata available after successful configuration setup."
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
      }),
      datatable$ui_dt(ns("metaDataReview"), "Meta Data Review", collapsed = TRUE)
    )
  )
}


#' @export
server_metadata <- function(id = "metadata") {
  box::use(.. / utilities / read / xlsx)
  box::use(.. / utilities / tables / datatable)
  box::use(shiny, dplyr, stats, bs4Dash, fs, shinyFiles, openxlsx, shinyWidgets)
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
            dplyr$rename(
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
            shiny$h3("2. Download and Set up Configuration.")
          ),
          shiny$column(
            6, shinyWidgets$prettySwitch(ns("internalConfig"),
              "Use Internal Configuration",
              value = TRUE
            )
          ),
          shiny$column(6, shiny$downloadButton(ns("downloadData"), "Download Configuration Template")),
          shiny$uiOutput(ns("configurationUploadToggle"), container = function(...) {
            shiny$column(12, ...)
          }),
          shiny$column(12, shiny$actionButton(ns("startAnalysis"), "Begin"))
        )
      })

      shiny$observeEvent(input$internalConfig, {
        output$configurationUploadToggle <- shiny$renderUI({
          # shiny$req(input$internalConfig)
          if (!isTRUE(input$internalConfig)) {
            shiny$p(
              shiny$h3("3. Upload a configuration file."),
              shiny$fileInput(
                inputId = ns("fileUpload"),
                label = "",
                accept = "*",
                multiple = TRUE
              )
            )
          }
        })
      })

      output$downloadData <- shiny$downloadHandler(
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        filename = "Config.xlsx",
        content = function(file) {
          file.copy("www/Config.xlsx", file)
        }
      )

      config <- shiny$eventReactive(input$startAnalysis, {
        if (input$internalConfig) {
          configPath <- getOption("internal_config_path")
        } else {
          configPath <- input$fileUpload$datapath
        }
        sheetNames <- openxlsx$getSheetNames(configPath)
        out <- lapply(sheetNames, function(sheetName) {
          list(
            sheetName = sheetName,
            data = openxlsx$read.xlsx(configPath, sheetName)
          )
        })
        clean_data <- clean$clean_config(out)
        clean_data <- dplyr$inner_join(clean_data, filteredData())
        if (nrow(clean_data) == 0) {
          shiny$showModal(shiny$modalDialog(
            "No analysis possible. Data not available for configuration in chosen directory."
          ))
          shiny$req(nrow(clean_data) > 0)
        }
        out <- list(
          clean = clean_data,
          raw = out
        )
        out
      })

      shiny$observeEvent(config(), {
        datatable$server_dt("metaDataReview", config()$clean)
      })

      config
    }
  )
}
