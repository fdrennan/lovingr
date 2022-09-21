#' @export
ui_metadata <- function(id = "metadata", width = 6) {
  box::use(shiny, bs4Dash, shinyFiles)
  box::use(.. / utilities / io / file_upload)
  box::use(.. / utilities / tables / datatable)
  ns <- shiny$NS(id)
  shiny$column(
    12,
    shiny$fluidRow(
      bs4Dash$box(
        title = "Data Manager", id = ns("dataImport"),
        closable = TRUE, collpased = FALSE, maximizable = TRUE, width = 12,
        shiny$fluidRow(
          shiny$column(
            12,
            shiny$p(
              shiny$h3("1. Import CSM Data from Filesystem"),
              shiny$div(
                class = "text-right",
                shinyFiles$shinyDirButton(ns("inputDir"), "Import",
                  "Please select a folder to import CSM data from.", FALSE,
                  class = "btn btn-default action-button"
                )
              )
            )
          ),
          shiny$uiOutput(ns("study"), container = function(...) {
            shiny$column(6, class = "py-3", ...)
          }),
          shiny$uiOutput(ns("year"), container = function(...) {
            shiny$column(6, class = "py-3", ...)
          }),
          shiny$uiOutput(ns("month"), container = function(...) {
            shiny$column(6, class = "py-3", ...)
          }),
          shiny$uiOutput(ns("analysis"), container = function(...) {
            shiny$column(6, class = "py-3", ...)
          })
        )
      ),
      shiny$uiOutput(ns("dataFilterManager"), container = function(...) {
        shiny$column(12, shiny$fluidRow(...))
      })
    )
  )
}


#' @export
server_metadata <- function(id = "metadata") {
  {
    box::use(.. / utilities / read / xlsx)
    box::use(.. / utilities / tables / datatable)
    box::use(shiny, dplyr, stats, bs4Dash, glue, fs)
    box::use(shinyFiles, openxlsx, shinyWidgets, purrr)
    box::use(.. / utilities / io / file_upload)
    box::use(.. / csm_config / clean)
    box::use(.. / cdm / meta)
  }
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shinyFiles$shinyDirChoose(input, id = "inputDir", roots = getOption("file_import_working_directory"))

      csmDataLocationsTable <- shiny$eventReactive(input$inputDir, {
        shiny$req(!inherits(input$inputDir, "shinyActionButtonValue"))

        file_path <- input$inputDir$path
        file_root <- input$inputDir$root
        file_path[[1]] <- NULL
        # file_path[[1]] = input$inputDir$root
        # file_path <- paste0(paste0(file_path, collapse="/"), collapse = '')
        # base_directory <- paste0(c(file_root, file_path), collapse = '')
        base_directory <- paste0(c(file_root, file_path), collapse = "/")
        shiny$showNotification(
          shiny$tags$pre(glue$glue("Searching for data in {base_directory}"))
        )
        # c(input$inputDir$root, input$inputDir$path)
        # base_directory <- file.path(input$inputDir$root, input$inputDir$path[[2]])

        csmDataLocationsTable <- meta$get_data(base_directory)
        csmDataLocationsTable
      })

      shiny$observeEvent(csmDataLocationsTable(), {
        output$dataFilterManager <- shiny$renderUI({
          bs4Dash$box(
            title = "Data", id = ns("metaDataFilterPanelBox"),
            closable = TRUE, collpased = FALSE, maximizable = TRUE, width = 12,
            shiny$uiOutput(ns("metaDataFilterPanel"), container = function(...) {
              shiny$fluidRow(shiny$column(
                12, ...
              ))
            })
          )
        })


        output$metaDataFilterPanel <- shiny$renderUI({
          shiny$fluidRow(
            shiny$uiOutput(ns("configurationUploadPanel"), container = function(...) {
              shiny$column(12, class = "py-3", ...)
            })
          )
        })

        output$study <- shiny$renderUI({
          study <- csmDataLocationsTable()$study
          shiny$selectizeInput(ns("study"), shiny$h5("Study"),
            choices = unique(study),
            selected = unique(study)[[1]],
            multiple = FALSE
          )
        })

        output$year <- shiny$renderUI({
          shiny$req(input$study)
          shiny$removeUI("#metadatawarning")
          year <- csmDataLocationsTable() |>
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
          csmDataLocationsTableFiltered <- dplyr$filter(csmDataLocationsTable(), study %in% input$study, year %in% input$year)
          monthName <- dplyr$pull(csmDataLocationsTableFiltered, monthName)
          max_month <- csmDataLocationsTableFiltered |>
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

          analysis <- csmDataLocationsTable() |>
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
      })

      filteredCsmDataTable <- shiny$reactive({
        shiny$req(input$analysis)
        csmDataLocationsTable <- csmDataLocationsTable()
        files <- csmDataLocationsTable |>
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
      })

      output$configurationUploadPanel <- shiny$renderUI({
        shiny$req(filteredCsmDataTable())
        shiny$fluidRow(
          shiny$column(
            12,
            shiny$h3("2. Download and Set up Configuration.")
          ),
          shiny$column(12,
            class = "d-flex justify-content-around align-items-center py-3",
            shinyWidgets$prettySwitch(ns("internalConfig"),
              "Use Internal Configuration",
              value = TRUE
            ),
            shiny$downloadButton(ns("downloadData"), "Download Configuration Template")
          ),
          shiny$uiOutput(ns("configurationUploadToggle"), container = function(...) {
            shiny$column(12, class = "py-3", ...)
          }),
          shiny$column(12, class = "py-3 text-right", bs4Dash$actionButton(ns("startAnalysis"), "Begin"))
        )
      })

      shiny$observeEvent(input$internalConfig, {
        output$configurationUploadToggle <- shiny$renderUI({
          if (!isTRUE(input$internalConfig)) {
            shiny$p(
              shiny$h3("3. Upload a configuration file."),
              shiny$fileInput(
                inputId = ns("fileUpload"),
                label = "",
                accept = "*",
                multiple = TRUE
              ),
              shinyWidgets$prettySwitch(ns("ignoreConfigDataPaths"),
                "Ignore Data Paths in Configuration",
                value = TRUE
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
        raw_config <- lapply(sheetNames, function(sheetName) {
          list(
            sheetName = sheetName,
            data = openxlsx$read.xlsx(configPath, sheetName)
          )
        })
        clean_data <- clean$clean_config(raw_config)
        clean_data <- dplyr$inner_join(clean_data, filteredCsmDataTable())
        if (nrow(clean_data) == 0) {
          shiny$showModal(shiny$modalDialog(
            "No analysis possible. Data not available for configuration in chosen directory."
          ))
          shiny$req(nrow(clean_data) > 0)
        }


        if (isFALSE(input$ignoreConfigDataPaths)) {
          clean_data$filepath <- NULL
          clean_data$filename <- NULL
          input_config_datapaths <- dplyr$rename(
            raw_config[[4]]$data,
            filepath = final
          ) |> dplyr$mutate(analysis = tolower(analysis))
          clean_data <- dplyr$inner_join(
            clean_data,
            input_config_datapaths
          )


          filepath_exists <- file.exists(clean_data$filepath)
          if (!all(filepath_exists)) {
            path_doesnt_exist <- unique(clean_data$filepath[!filepath_exists])
            shiny$showModal(
              shiny$modalDialog(
                shiny$fluidRow(
                  shiny$column(
                    12, shiny$h4("Data not found"),
                    shiny$tags$pre(
                      paste0(path_doesnt_exist, collapse = "\n")
                    )
                  )
                )
              )
            )
          }
          shiny$req(FALSE)
        }
        bs4Dash$updateBox(id = "dataImport", action = "remove")
        bs4Dash$updateBox(id = "metaDataFilterPanelBox", action = "remove")

        out <- list(
          clean = clean_data,
          raw = raw_config
        )
        out
      })



      config
    }
  )
}
