

#' @export
clean_config <- function(config) {
  box::use(dplyr, purrr)

  unescape_html <- function(str) {
    box::use(xml2, rvest)
    xml2$xml_text(rvest$read_html(paste0("<x>", str, "</x>")))
  }

  flagging_sheet <- purrr$keep(config, ~ .$sheetName == "Flagging")[[1]]
  datapaths <- purrr$keep(config, ~ .$sheetName == "DataPaths")[[1]]

  flagging_sheet <- flagging_sheet$data |>
    dplyr$select(Analysis, Signals, Flagging.Specification) |>
    dplyr$filter(!is.na(Flagging.Specification)) |>
    dplyr$group_by_all() |>
    dplyr$mutate(cur_group_id = dplyr$cur_group_id())


  flagging_sheet <- split(
    flagging_sheet,
    with(flagging_sheet, cur_group_id)
  )

  flagging_sheet <- purrr$map_dfr(flagging_sheet, function(x) {
    x$Flagging.Specification <- unescape_html(x$Flagging.Specification)
    data.frame(
      analysis = tolower(x$Analysis),
      paramcd = tolower(strsplit(x$Signals[1], ", ")[[1]]),
      flagging_specification = x$Flagging.Specification
    )
  })
  flagging_sheet <- dplyr$inner_join(flagging_sheet, datapaths$data)
}
