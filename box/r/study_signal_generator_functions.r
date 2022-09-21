
# ADDL FUNCTIONS
#' remap_rename
#' @export remap_rename
remap_rename <- function(analysis_results, meta_data, n_sub_information) {
  analysis_results <- map(
    analysis_results,
    function(x) {
      if (!"site_avg" %in% names(x)) {
        x$site_avg <- NA
      }

      if (!"stdy_avg" %in% names(x)) {
        x$stdy_avg <- NA
      }
      if ("signals" %in% names(x)) {
        x <- mutate(x, paramcd = signals)
      }

      if ("Par" %in% names(x)) {
        x <- mutate(x, paramcd = Par)
        x <- mutate(x, signal = Par)
      }
      if ("signal" %in% names(x)) {
        x <- mutate(x, paramcd = signal)
      }

      if ("siteid" %in% names(x)) {
        x <- mutate(x, site = as.character(siteid))
      }
      if ("site" %in% names(x)) {
        x <- mutate(x, site = as.character(site))
      }
      if ("grp_id" %in% names(x)) {
        x <- mutate(x, site = as.character(grp_id))
      }
      x
    }
  )


  analysis_results <-
    map(
      analysis_results,
      function(x) {
        x$country <- NULL
        x$cutdt <- NULL
        print(x)
        if ("siteid" %in% colnames(x)) {
          x$site <- x$siteid
          x$siteid <- NULL
        }
        x$site <- as.character(x$site)
        x$site <- Vectorize(CONVERT_TO_SITE)(x$site)
        x <- inner_join(
          x,
          distinct(transmute(meta_data, site = as.character(siteid), cutdt = cutdt, country)),
          by = "site"
        )
        x$n_subj <- NULL
        x <- inner_join(x, n_sub_information)
        x <- mutate_at(x, vars(contains("p_value")), ~ round(., 3))
        x <- mutate(x, cutdt = tryCatch(
          {
            as.Date(cutdt, origin = "1960-1-1")
          },
          error = function(err) {
            if (cutdt[1] == "24-oct-16") {
              cutdt <- rep(as.Date("2016-08-24"), length(cutdt))
              return(cutdt)
            }
            anydate(cutdt)
          }
        ))
        x <- mutate(x, time_run = Sys.time())
        x <- select(x, time_run, everything())
        x
      }
    )
}

#' write_plot_files
#' @export write_plot_files
write_plot_files <- function(mastersheet, repository, outpath) {
  scbd_path <- dir_ls(repository, recurse = TRUE, regexp = "xlsx")
  scbd_path <- scbd_path[!str_detect(scbd_path, "/-")]
  scbd <- get_scoreboard(scbd_path)
  gv_plots(scbd,
    outpath = repository, dups_tbl = FALSE,
    paste0(unique(mastersheet$data_paths$base), "/csmrgv.sas7bdat")
  )
  vs_plots(scbd,
    outpath = repository,
    data_path = filter(mastersheet$data_paths, analysis == "vitals")$final
  )
}

#' BUILD_META_DATA
#' @export BUILD_META_DATA
BUILD_META_DATA <- function(meta_data) {
  n_subj <- distinct(meta_data, studyid, subject, siteid) %>%
    group_by(studyid, siteid) %>%
    count(name = "n_subj")
  n_subj_visit <- distinct(meta_data, siteid, subject, numvisit) %>%
    group_by(siteid) %>%
    summarise(n_subj_visit = sum(numvisit, na.rm = TRUE))


  n_sub_information <- inner_join(n_subj, n_subj_visit) %>%
    mutate(site = as.character(siteid))
  n_sub_information$siteid <- NULL
  n_sub_information
}

#' write_excel_file
#' @export write_excel_file
write_excel_file <- function(analysis_results, scoreboard, output_dir, outpath) {
  ## Create a new workbook
  wb <- createWorkbook()
  outpath <- file.path(output_dir, "SCOREBOARD")
  style <- createStyle(
    wrapText = FALSE, fontSize = 12,
    halign = "left",
    # fgFill = "#4F81BD",
    border = "TopBottom"
    # borderColour = "#4F81BD"
  )
  ## Save workbook to working directory

  iwalk(
    analysis_results,
    function(data, tab) {
      addWorksheet(wb, sheetName = tab)
      # data <- data[complete.cases(data),]
      addStyle(wb, tab, style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE, stack = FALSE)
      data <- mutate_if(data, is.numeric, ~ round(., 3))
      cols_widths <- map_dbl(data, ~ max(min(nchar(.), 40), 5))
      names_widths <- map_dbl(names(data), ~ max(min(nchar(.), 90), 5))
      min_width <- map2_dbl(names_widths, cols_widths, function(x, y) max(x, y))
      col_index <- 1:length(cols_widths)
      setColWidths(wb, tab, cols = col_index, widths = min_width + 6)
      writeDataTable(wb, sheet = tab, data, tableStyle = "TableStyleLight9")
    }
  )

  if (length(scoreboard$site)) {
    addWorksheet(wb, "Scoreboard")
    scoreboard$`Score the Signals \n1. Action taken \n2. Useful but no action \n3. Not useful` <- ""
    scoreboard$Comments <- ""
    message("adding style")
    addStyle(wb, "Scoreboard", style, 1, 1:20)

    names_widths <- map_dbl(names(scoreboard), ~ max(min(nchar(.), 80), 5))
    cols_widths <- map_dbl(scoreboard, ~ max(min(nchar(as.character(.)), 80), 5))
    min_width <- map2_dbl(names_widths, cols_widths, function(x, y) max(x, y))
    col_index <- 1:length(cols_widths)
    setColWidths(wb, "Scoreboard", cols = col_index, widths = min_width + 6)

    writeDataTable(wb, "Scoreboard", scoreboard, tableStyle = "TableStyleLight9")
  }

  # setHeader(wb, "On the right", position = "right")

  try(dir_create(outpath, recurse = TRUE))

  filename <- file.path(outpath, paste0(analysis_results$aei$cutdt[[1]], "-SCOREBOARD.xlsx"))
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}

#' store_csv_file
#' @export store_csv_file
store_csv_file <- function(response, outpath) {
  response$analysis_results$scoreboard_raw <- bind_rows(response$scoreboard_raw)
  cutdt <- response$analysis_results[[1]]$cutdt[[1]]
  response$analysis_results <- keep(response$analysis_results, ~ nrow(.) > 0)
  iwalk(
    response$analysis_results,
    function(x, y) {
      print(y)
      if (y == "aei") {
        outpath <- file.path(outpath, "AE")
      }
      if (y == "diet") {
        outpath <- file.path(outpath, "DIET")
      }
      if (y == "aecnt") {
        outpath <- file.path(outpath, "AE")
      }
      if (y == "aegap") {
        outpath <- file.path(outpath, "AE")
      }
      if (y == "underdose") {
        outpath <- file.path(outpath, "DOSE")
      }
      if (y == "vitals") {
        outpath <- file.path(outpath, "VITALS")
      }
      if (y == "rgm") {
        outpath <- file.path(outpath, "RGM")
      }
      if (y == "rgv") {
        outpath <- file.path(outpath, "RGV")
      }
      if (y == "rgv") {
        outpath <- file.path(outpath, "RGV")
      }

      if (y == "retention") {
        outpath <- file.path(outpath, "RETENTION")
      }

      if (y == "scoreboard") {
        outpath <- file.path(outpath, "SCOREBOARD")
      }

      if (y == "scoreboard_raw") {
        outpath <- file.path(outpath, "SCOREBOARD_RAW")
      }

      # if (is.null(x$cutdt)) {
      #   x$cutdt = 'missing-cutdt'
      # }
      #

      y <- paste0(unique(x$cutdt), "_", y)

      if (!dir_exists(outpath)) {
        dir_create(outpath)
      }

      write_csv(x, paste0(file.path(outpath, y), ".csv"))
    }
  )
}

#' APPLY_GROUP_FLAG
#' @export APPLY_GROUP_FLAG
APPLY_GROUP_FLAG <- function(analysis_results) {
  analysis_results <- map(
    analysis_results,
    function(x) {
      x <-
        x %>%
        group_by(site) %>%
        mutate(SiteFlagged = any(flag != 0)) %>%
        ungroup()

      x <-
        x %>%
        group_by(site, paramcd) %>%
        mutate(ParamSiteFlagged = any(flag != 0)) %>%
        ungroup()

      x
    }
  )
}

#' CONVERT_TO_SITE
#' @export CONVERT_TO_SITE
CONVERT_TO_SITE <- function(x) {
  n_space <- 4 - nchar(x)
  paste0(paste0(rep(0, n_space), collapse = ""), x)
}

#' #' select_analysis_columns_to_keep
#' #' @export select_analysis_columns_to_keep
#' select_analysis_columns_to_keep <- function(analysis_results) {
#'
#'
#'   analysis_results <-
#'     analysis_results %>%
#'     map(remove_empty)
#'
#'   analysis_results
#' }

#' grab_meta_data
grab_meta_data <- function(mastersheet) {
  meta_path <- filter(mastersheet$data_paths, analysis == "meta")$final

  if (!length(meta_path)) {
    stop("meta path missing from data paths in excel file")
  }

  meta_data <- csm_data_load(meta_path)
  n_sub_information <- BUILD_META_DATA(meta_data)
  list(meta_data = meta_data, n_sub_information = n_sub_information)
}

#' separate_analysis_errors
#' @export separate_analysis_errors
separate_analysis_errors <- function(analysis_results) {
  analysis_error_filter <-
    map_lgl(analysis_results, function(x) {
      if (is.null(x$analysis)) {
        x$analysis <- "not required"
      }

      if ("error" %in% names(x) & unique(x$analysis) != "retention") {
        csm_cli_header(glue("Error in {x$analysis}"))
        message(x$error)
        message("Dropping from scoreboard")
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
  analysis_errors <- analysis_results[!analysis_error_filter]
  if ("retention" %in% names(analysis_results)) {
    if ("error" %in% names(analysis_results$retention)) {
      analysis_errors$retention <- filter(analysis_results$retention, !is.na(error))
      analysis_results$retention <- filter(analysis_results$retention, is.na(error))
    }
  }
  analysis_results <- analysis_results[analysis_error_filter]
  analysis_results <- keep(analysis_results, ~ nrow(.) > 0)
  list(analysis_results = analysis_results, analysis_errors = analysis_errors)
}

#' verify_path_integrity
#' @export verify_path_integrity
verify_path_integrity <- function(mastersheet, analysis_filter) {
  incorrect_paths <- mastersheet$data_paths %>%
    filter(!file_exists(mastersheet$data_paths$final)) %>%
    select(analysis, final)

  incorrect_paths <- incorrect_paths[incorrect_paths$analysis %in% analysis_filter, ]

  if (nrow(incorrect_paths) > 0) {
    message("Please review the following file paths. They do not exist.")

    print(incorrect_paths)
    response <- readline("The above file paths do not exist, would you like to quit? Type q to quit. Otherwise, hit any button.")
    if (response == "q") {
      stop("You have chosen to stop the application.")
    }
  }
}
