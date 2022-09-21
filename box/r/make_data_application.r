#' MAKE_DATA_APPLICATION
#' @export MAKE_DATA_APPLICATION
MAKE_DATA_APPLICATION <- function(DIRECTORY_BASE) {
  cli_alert_info("Within MAKE_DATA_APPLICATION at {DIRECTORY_BASE}")

  FILES_SCOREBOARD <-
    DIRECTORY_BASE %>%
    dir_info(recurse = TRUE, regexp = "scoreboard_raw") %>%
    mutate(
      name = path_file(path),
      cutdt = substr(name, 1, 10),
      scoreboard_button_name = paste0("Scoreboard: ", cutdt)
    ) %>%
    filter(!grepl("cache", path), path_file(name) != "_scoreboard_raw.csv")

  FILES_SCOREBOARD_EXCEL <-
    DIRECTORY_BASE %>%
    dir_info(recurse = TRUE, regexp = "SCOREBOARD.xlsx") %>%
    mutate(
      name = path_file(path),
      cutdt = substr(name, 1, 10),
      scoreboard_button_name = paste0("Scoreboard: ", cutdt)
    )

  SCOREBOARDS <- map(FILES_SCOREBOARD$path, read_csv, show_col_types = FALSE)
  SCOREBOARDS <- keep(SCOREBOARDS, ~ length(.) > 0)

  DATA_APPLICATION_RAW <- map2(
    split(FILES_SCOREBOARD, FILES_SCOREBOARD$path),
    SCOREBOARDS,
    ~ list(META = ..1, DATA = ..2)
  )

  DATA_APPLICATION_COMPLETE <-
    map(
      DATA_APPLICATION_RAW,
      BIND_DATA_AND_META
    )

  scoreboard <- openxlsx::read.xlsx(FILES_SCOREBOARD_EXCEL$path, "Scoreboard")

  DATA_APPLICATION_COMPLETE[[1]]$DATA <-
    inner_join(
      DATA_APPLICATION_COMPLETE[[1]]$DATA,
      transmute(scoreboard, cutdt = ymd(cutdt), potential_issue, country, site, Summary.Statistics)
    ) %>%
    mutate(
      base_dir = str_remove(path_dir(path), "SCOREBOARD_RAW"),
      app_data_dir = paste0(base_dir, "app")
    ) %>%
    mutate(
      file_path = glue("{app_data_dir}/{studyid}/{cutdt}-{site}-{country}-{scoreboard_group_id}.rds")
    ) %>%
    rowwise() %>%
    mutate(dir_created = dir_create(app_data_dir, recurse = TRUE))
  list(DATA_APPLICATION_COMPLETE)
}
