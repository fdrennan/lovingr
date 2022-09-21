# Generate guide to help understand table - guide to summary statistics
# map plots to table

#' grab_data
#' @export grab_data
grab_data <- function(DIRECTORY_BASE = NULL, input = NULL, scoreboard_list = NULL) {
  if (is.null(scoreboard_list)) {
    scoreboard_list <-
      dir_info(DIRECTORY_BASE, recurse = T, regexp = "SCOREBOARD_RAW", type = "file") %>%
      mutate(date = str_remove(path_file(path), "_scoreboard_raw.csv")) %>%
      filter(ymd(date) >= Sys.Date() - months(5))

    # debug(MAKE_DATA_APPLICATION)
    scoreboard_list <-
      scoreboard_list %>%
      select(path) %>%
      mutate(home_path = str_remove(path, str_extract(path, "/SCOREBOARD_RAW.+csv"))) %>%
      rowwise()

    scoreboard_list <-
      scoreboard_list %>%
      mutate(
        data_for_application = tryCatch(expr = {
          list(MAKE_DATA_APPLICATION(home_path))
        }, error = function(err) {
          list(as.character(err))
        })
      )

    scoreboard_list <-
      scoreboard_list %>%
      filter(!is.character(data_for_application)) %>%
      mutate(
        meta_data = map(data_for_application, ~ .[[1]]$META),
        raw_data = map(data_for_application, ~ .[[1]]$DATA)
      )

    scoreboard_list <-
      scoreboard_list %>%
      select(-path) %>%
      mutate(plot_locations = map(home_path, pull_plot_locations)) %>%
      unnest(c(meta_data))

    scoreboard_list <-
      scoreboard_list %>%
      rowwise() %>%
      mutate(
        raw_sub = SUBSET_APPLICATION_DATA(raw_data)
      )
  }

  if (!is.null(input)) {
    filter(
      scoreboard_list,
      cutdt == input$cutdt, studyid == input$study
    )
  } else {
    scoreboard_list
  }
}

#' pull_plot_locations
#' @export pull_plot_locations
pull_plot_locations <- function(DIRECTORY_BASE = NULL) {
  plot_locations <- dir_info(DIRECTORY_BASE, regexp = ".jpg")
  plot_locations <-
    plot_locations %>%
    select(path) %>%
    mutate(name = path_file(path)) %>%
    separate(col = name, into = c("site", "country", "paramcd"), sep = "-") %>%
    mutate(paramcd = str_remove(paramcd, ".jpg")) %>%
    arrange(country, site, paramcd) %>%
    rename(path_plot = path)

  plot_locations
}

#' attach_stored_app_data
#' @export attach_stored_app_data
attach_stored_app_data <- function(app_data, dfr = FALSE) {
  if (dfr) {
    out <- map_dfr(
      split(app_data, app_data$index),
      function(x) {
        fp <- x$file_path[[1]]
        if (is_file(fp)) {
          cli_alert_info("Grabbing file {fp}")
          stored_data <- read_rds(fp)
          x$comment <- NULL
          x$action <- NULL
          y <- left_join(x, stored_data)
          return(y)
        }
        x
      }
    )
  } else {
    out <- map(
      app_data,
      function(x) {
        fp <- x$file_path[[1]]
        if (is_file(fp)) {
          cli_alert_info("Grabbing file {fp}")
          stored_data <- read_rds(fp)

          y <- left_join(x, stored_data)
          return(y)
        }
        x
      }
    )
  }
}

#' expand_for_display
#' @export expand_for_display
expand_for_display <- function(app_data) {
  if (!"path_plot" %in% colnames(app_data)) {
    cli_alert_warning("path_plot missing")
    app_data$path_plot <- "none"
  }
  app_data <-
    app_data %>%
    select(-cutdt, -studyid, -path) %>%
    unnest(raw_sub) %>%
    unnest(raw_sub) %>%
    split(.$index)

  app_data
}
