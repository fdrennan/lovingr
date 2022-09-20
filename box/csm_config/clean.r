

#' @export
clean_config <- function(config) {
  box::use(dplyr, purrr, shiny, styler)

  unescape_html <- function(str) {
    box::use(xml2, rvest)
    xml2$xml_text(rvest$read_html(paste0("<x>", str, "</x>")))
  }

  flagging_sheet <- purrr$keep(config, ~ .$sheetName == "Flagging")[[1]]
  # datapaths <- purrr$keep(config, ~ .$sheetName == "DataPaths")[[1]]

  flagging_sheet <- flagging_sheet$data |>
    dplyr$select(Analysis, Signals, Flagging.Specification, Flag) |>
    dplyr$filter(!is.na(Flagging.Specification)) |>
    dplyr$group_by_all() |>
    dplyr$mutate(cur_group_id = dplyr$cur_group_id())

  flagging_sheet <- split(
    flagging_sheet,
    with(flagging_sheet, cur_group_id)
  )
  flagging_sheet <- purrr$map_dfr(flagging_sheet, function(x) {
    x$Flagging.Specification <- unescape_html(x$Flagging.Specification)
    tryCatch(
      {
        x$Flagging.Specification <- styler$style_text(x$Flagging.Specification)
      },
      error = function(err) {
        shiny$showModal(shiny$modalDialog(
          size = "xl", easyClose = FALSE, fade = FALSE,
          shiny$fluidRow(
            shiny$column(12,
              class = "text-center",
              shiny$h4("Error in flagging code, please correct and try again.")
            ),
            shiny$column(
              12,
              shiny$tags$p(
                shiny$tags$pre(err$message, class = "py-3")
              )
            )
          )
        ))
        Sys.sleep(15)
      }
    )
    data.frame(
      analysis = tolower(unique(x$Analysis)),
      paramcd = tolower(strsplit(unique(x$Signals), ", ")[[1]]),
      flagging_code = unique(x$Flagging.Specification),
      flagging_value = as.numeric(unique(x$Flag))
    )
  })
  flagging_sheet
}
