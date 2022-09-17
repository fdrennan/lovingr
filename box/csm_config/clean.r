

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
  print(dplyr$glimpse(flagging_sheet))
  flagging_sheet <- purrr$map_dfr(flagging_sheet, function(x) {
    x$Flagging.Specification <- unescape_html(x$Flagging.Specification)
    print(x$Flagging.Specification)
    x$Flagging.Specification <- styler$style_text(x$Flagging.Specification)
    print(x$Flagging.Specification)
    data.frame(
      analysis = tolower(unique(x$Analysis)),
      paramcd = tolower(strsplit(unique(x$Signals), ", ")[[1]]),
      flagging_code = unique(x$Flagging.Specification),
      flagging_value = as.numeric(unique(x$Flag))
    )
  })

  # flagging_sheet <- dplyr$inner_join(flagging_sheet, datapaths$data)
  flagging_sheet
}
