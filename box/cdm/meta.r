#' @export get_data
get_data <- function(base_directory) {
  message("Loading Libraries")
  box::use(
    openxlsx, fs, dplyr, purrr, stringr,
    lubridate, cli, tictoc, stats, shiny, glue
  )

  analysis_dataset_regex <- {
    adn <- getOption("analysis_dataset_names")
    adn <- paste0(adn, collapse = "|")
    adn <- gsub("\\.", "\\\\.", adn)
    adn <- paste0("(", adn, ")")
  }

  subfiles <- fs$dir_info(
    base_directory,
    fail = FALSE,
    recurse = TRUE,
    type = "file",
    regexp = analysis_dataset_regex
  )
  subfiles <-
    subfiles |>
    dplyr$mutate(
      period = stringr$str_extract(path, "csm[0-9]{6}[a|b|c]"),
      date = stringr$str_remove(stringr$str_extract(path, "csm[0-9]{6}"), "csm"),
      year = stringr$str_sub(date, 1, 4),
      month = stringr$str_sub(date, 5, 6),
      monthName = month.name[as.numeric(month)],
      size_hr = fs$fs_bytes(size),
      filename = fs$path_file(path),
      date = lubridate$make_date(year, month),
      study = stringr$str_extract(path, "/[0-9]{6}/"),
      study = stringr$str_remove_all(study, "/")
    )

  subfiles
}
