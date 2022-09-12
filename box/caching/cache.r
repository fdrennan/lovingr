#' @export
check <- function() {
  box::use(readr, dplyr)
  cache_path <- getOption("cache_path")
  # browser()
  if (getOption("cache")) {
    analysis_filter <- getOption("analysis_filter")
    data <- readr$read_rds(cache_path)
    data <- data |> dplyr$filter(!is.na(analysis))
    if (!is.null(analysis_filter)) {
      data <- data |> dplyr$filter(analysis %in% analysis_filter)
    }
    data
  } else {
    box::use(.. / cdm / meta)
    data <- meta$get_data()
    readr$write_rds(data, cache_path)
    data
  }
}
