#' @export get_data
get_data <- function(base_directory = getwd()) {
  box::use(
    openxlsx, fs, dplyr, purrr, stringr,
    lubridate, cli, tictoc, stats, shiny, glue
  )

  analysis_dataset_names <- getOption("analysis_dataset_names")
  analysis_dataset_regex <- {
    adn <- analysis_dataset_names
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
    dplyr$transmute(
      date = stringr$str_remove(stringr$str_extract(path, "csm[0-9]{6}"), "csm"),
      year = stringr$str_sub(date, 1, 4),
      month = stringr$str_sub(date, 5, 6),
      monthName = month.name[as.numeric(month)],
      size_hr = fs$fs_bytes(size),
      filename = fs$path_file(path),
      date = lubridate$make_date(year, month),
      study = stringr$str_remove_all(stringr$str_extract(path, "/[0-9]{6}/"), "/"),
      path
    )
  subfiles$analysis <- names(analysis_dataset_names[match(subfiles$filename, analysis_dataset_names)])

  # Attach information about code to be executed with the analysis
  {
    analysis_code_files <- fs$dir_info(
      "box/analysis/modules",
      recurse = FALSE,
      type = "directory"
    ) |>
      dplyr$transmute(
        analysis = fs$path_file(path),
        analysis_code_path = path,
        csm_version = options("csm_version"),
        current_time = Sys.time()
      )
  }
  # 
  subfiles <- dplyr$inner_join(subfiles, analysis_code_files)
  subfiles
}
