#' @export
ui_devop <- function(id = "devop") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$inputPanel(
    shiny$actionButton(
      "deleteDevelopmentFolder",
      "Delete Develoment Folder"
    ),
    shiny$actionButton(
      "createDevelopmentFolder",
      "Create Develoment Folder"
    )
  )
}

#' @export
server_devop <- function(id = "devop") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      shiny$observeEvent(input$deleteDevelopmentFolder, {
        fs$dir_delete(getOption("datamisc_cache_path"))
      })
      shiny$observeEvent(input$createDevelopmentFolder, {
        shiny$req(filteredData())
        data <- filteredData()
        datamisc_cache_path <- getOption("datamisc_cache_path")

        data <- data |>
          dplyr$rowwise() |>
          dplyr$mutate(
            local_path = fs$path_join(c(datamisc_cache_path, path))
          )
        data <- data |>
          dplyr$filter(!file.exists(local_path))


        if (nrow(data)) {
          n_files <- length(data$local_path)
          data <- data |>
            dplyr$mutate(
              create_dir = fs$dir_create(fs$path_dir(local_path), recurse = T),
              copied = file.copy(path, local_path, overwrite = FALSE)
            ) |>
            dplyr$ungroup()
        }
      })
    }
  )
}
