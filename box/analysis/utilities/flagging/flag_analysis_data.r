#' @export
flag_analysis_data <- function(data, metadata) {
  box::use(dplyr)
  data <- dplyr$inner_join(metadata, data)
  data |>
    dplyr$rowwise() |>
    dplyr$mutate(
      is_flagged = eval(parse(text = flagging_code))
    ) |>
    dplyr$filter(is_flagged) |>
    dplyr$distinct()
}
