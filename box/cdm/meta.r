#' @export get_data
get_data <- function() {
  message("Loading Libraries")
  box::use(
    openxlsx, fs, dplyr, purrr, stringr,
    lubridate, cli, tictoc, stats, shiny, glue
  )
  file_regex <- "csm[0-9]{6}[a|b|c]/datamisc$"
  base_directory <- "/sassys/cdm/cdmdev/bmn111/ach"
  cli$cli_alert("Pattern: {.path {file_regex}}")
  cli$cli_alert("Directory: {.path {base_directory}}")
  tictoc$tic()
  datamisc_folders <- fs$dir_info(
    base_directory,
    fail = FALSE,
    recurse = TRUE,
    regexp = file_regex,
    type = "directory"
  )
  cli$cli_alert("Grabbing datamisc files")
  tictoc$toc()
  datamisc_files <- purrr$map_dfr(
    split(datamisc_folders, datamisc_folders$path),
    function(path) {
      with(
        path,
        {
          cli$cli_alert("Importing {.path {path}}")
          fs$dir_info(path, type = "file")
        }
      )
    }
  ) |>
    dplyr$mutate(
      filename = fs$path_file(path)
    )

  datamisc_files <-
    dplyr$inner_join(
      datamisc_files,
      {
        base_config <- getOption('base_config')
        cli$cli_alert("Reading {base_config} for analysis to filename match.")
        metapaths <- openxlsx$read.xlsx(base_config, 4)[, c("analysis", "filename")]
      },
      by = "filename"
    )

  cli$cli_alert("Extracting date from filepath")
  datamisc_files <-
    datamisc_files |>
    dplyr$mutate(
      period = stringr$str_extract(path, "csm[0-9]{6}[a|b|c]"),
      date = stringr$str_remove(stringr$str_extract(path, "csm[0-9]{6}"), "csm"),
      year = stringr$str_sub(date, 1, 4),
      month = stringr$str_sub(date, 5, 6),
      monthName = month.name[as.numeric(month)],
      size_hr = fs$fs_bytes(size)
    )

  cli$cli_alert("Keeping what we need")
  datamisc_files <- datamisc_files |>
    dplyr$mutate(
      date = lubridate$make_date(year, month),
      study = stringr$str_extract(path, "/[0-9]{6}/"),
      study = stringr$str_remove_all(study, "/")
    )


  datamisc_files
}
