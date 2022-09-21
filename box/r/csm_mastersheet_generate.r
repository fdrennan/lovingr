#' csm_mastersheet_global
#' @param path_csm_excel
#' @param name_var
#' @export csm_mastersheet_global
csm_mastersheet_global <- function(path_csm_excel = NULL, name_var = NULL, sheet = 4) {
  data <- openxlsx::read.xlsx(path_csm_excel, sheet = sheet)
  filter(data, name == name_var)$value
}


#' csm_mastersheet_generate
#'
#'
#'
#' @importFrom dplyr filter
#' @importFrom dplyr rename_all
#' @importFrom tibble tibble
#' @importFrom stringr str_squish
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_lower
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom dplyr inner_join
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom purrr keep
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param excel_path Defaults to "CSMConfigMastersheet.xlsx", in the working directory.
#'
#' @description
#'
#' Given a path to the CSM Excel file, `csm_mastersheet_generate`
#' returns the tabs in a clean, list of data frames.
#'
#'
#' ## Application Dataset Standards
#'
#' Each analysis is developed off of a development dataset. The datasets
#' used for each analysis are listed below.
#' ````
#' aei: /111302/csm02012x/datamisc/csmaest.sas7bdat
#' rgm: /111302/csm02012x/datamisc/summary.sas7bdat
#' vitals: /ach/111302/csm02012x/datamisc/csmvs.sas7bdat
#' dosereason: /bmn111/ach/111302/csm02012x/datamisc/csmexvis.sas7bdat
#' overdose: /bmn111/ach/111302/csm02012x/datamisc/csmexvis.sas7bdat
#' underdose: /bmn111/ach/111302/csm02012x/datamisc/csmexvis.sas7bdat
#' rgv: /bmn111/ach/111302/csm02012x/datamisc/rinput302l.sas7bdat
#' aegap: /bmn111/ach/111302/csm02012x/datamisc/csmae.sas7bda
#' aecnt: /bmn111/ach/111302/csm02012x/output/clin/tab/AE/csm_aecnt_111302_20210209.csv
#' meta: /bmn111/ach/111302/csm02012x/datamisc/csmpt.sas7bdat
#' ```
#'
#' @md
#'
#' @family study_signal_generator
#' @export csm_mastersheet_generate
csm_mastersheet_generate <- function(excel_path = "CSMConfigMastersheet-master.xlsx") {
  html_vector <- Vectorize(unescape_html)

  sheetnames <- openxlsx::getSheetNames(excel_path)
  dataframes <- map(set_names(sheetnames, sheetnames), ~ read.xlsx(excel_path, .))
  names(dataframes) <- snakecase::to_snake_case(names(dataframes)) %>%
    map_chr(
      ~ {
        case_when(
          . == "program_configuration_parameter" ~ "analysis_variables",
          . == "flagging" ~ "flagging_setup",
          . == "scoreboard" ~ "scoreboard_setup",
          TRUE ~ .
        )
      }
    )

  # names(dataframes) <- c("analysis_variables", "flagging_setup", "scoreboard_setup", "data_paths")
  dataframes_clean <- map(dataframes, normalize)
  dataframes_clean$data_paths <- dataframes$data_paths
  dataframes <- dataframes_clean

  # create response ---------------------------------------------------------
  dataframes$signals <- filter(
    .data = dataframes$flagging_setup,
    !is.na(.data$study),
    !is.na(.data$category), !is.na(.data$analysis),
    !is.na(signals)
  )

  dataframes$signals$group_id <- dataframes$flagging_setup %>%
    group_by(study, category, analysis, signals) %>%
    group_indices()

  # split response ----------------------------------------------------------
  dataframes$signals <- split(
    dataframes$signals,
    dataframes$signals$group_id
  )

  # create response tibble --------------------------------------------------
  # This maps the first element in study, flagvar, category, datacutoff,
  # analysis and signals (after splitting and unlisitng (i.e. 'shifting') each
  # element separated by a comma
  dataframes$signals <-
    map(
      dataframes$signals,
      function(x = NULL) {
        x <- tibble(
          study = x$study[[1]],
          category = x$category[[1]],
          analysis = x$analysis[[1]],
          signals = str_squish(unlist(str_split(x$signals, ",")))
        )
        # get distinct rows in response
        distinct(x)
      }
    )


  # create (Flagging) data from signals -------------------------------------
  dataframes$flagging_setup <-
    dataframes$flagging_setup %>%
    # recall data comes from the Flagging sheet. Here we select the following
    # columns
    select(.data$study, .data$analysis, .data$flagging_specification, .data$signals) %>%
    # and split this dataset on the signals column
    split(.$signals) %>%
    map_df(
      # these 'split' tibbles then get passed into a function which splits them
      # on the the values stored in signals[[1]]
      function(x) {
        signal_loop_data <- str_trim(str_split(x$signals[[1]], ",")[[1]])
        map_df(
          signal_loop_data,
          function(y) {
            x$signals <- y
            x
          }
        )
      }
    )

  dataframes$flagging_setup <- map(
    dataframes$signals,
    function(x) {
      unescape_html <- Vectorize(unescape_html)
      r_code <- inner_join(x, select(dataframes$flagging_setup, .data$study, .data$analysis, .data$flagging_specification, .data$signals), by = c("study", "analysis", "signals"))
      r_code <- distinct(r_code, .data$study, .data$signals, .data$flagging_specification)
      r_code <- filter(r_code, !is.na(.data$flagging_specification))
      r_code <- group_by(r_code, .data$study, .data$signals)
      r_code <- summarise(.data = r_code, flagging_specification = paste0(.data$flagging_specification, collapse = " "))
      r_code <- mutate(r_code, flagging_specification = unescape_html(.data$flagging_specification))
      r_code <- inner_join(x, r_code, by = c("study", "signals"))
      if (nrow(r_code) == 0) {
        return(FALSE)
      }
      mutate(
        r_code,
        code = str_replace_all(.data$flagging_specification, "#", .data$signals)
      )
    }
  )

  dataframes$flagging_setup <- keep(dataframes$flagging_setup, function(x) !inherits(x, "logical"))
  dataframes$flagging_setup <- bind_rows(dataframes$flagging_setup)
  dataframes$flagging_setup <- mutate_if(dataframes$flagging_setup, is.character, str_to_lower)

  ########################
  # clean names -------------------------------------------------------------
  dataframes$scoreboard_setup <- dataframes$scoreboard_setup %>% janitor::clean_names()

  if (!"signal_subgroup" %in% names(dataframes$scoreboard_setup)) {
    dataframes$scoreboard_setup$signal_subgroup <- NA
  }
  dataframes$scoreboard_setup <- dataframes$scoreboard_setup %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate_if(is.character, str_trim) %>%
    transmute(
      potential_issue,
      potential_issue_subfix,
      signal_subgroup,
      analysis = analysis_type,
      flag = as.numeric(signal_flag_value),
      max_number_signal_summary,
      sitediff_vs_study,
      signal_prefix, signal_subfix,
      name_of_endpoint_of_interest,
      site_stat,
      study_stat
    )

  dataframes$scoreboard_setup <- dataframes$scoreboard_setup %>%
    mutate(
      max_number_signal_summary = if_else(
        # !! check with cheng about default is missing! ----
        is.na(max_number_signal_summary), 1e10, max_number_signal_summary
      ),
      signal_prefix = if_else(
        is.na(signal_prefix), "", signal_prefix
      ),
      signal_subfix = if_else(
        is.na(signal_subfix), "", signal_subfix
      )
    )
  #######################
  #######################

  dataframes$scoreboard_setup <- dataframes$scoreboard_setup %>%
    mutate_if(is.character, ~ str_trim(str_to_lower(..1)))

  dataframes$scoreboard_setup <-
    dataframes$scoreboard_setup %>%
    mutate(scoreboard_group_id = row_number())

  if ("signal_subgroup" %in% names(dataframes$scoreboard_setup)) {
    dataframes$scoreboard_setup <-
      dataframes$scoreboard_setup %>%
      split(~ .$scoreboard_group_id) %>%
      map_dfr(function(x) {
        if (!is.na(x$signal_subgroup)) {
          bind_cols(
            select(x, -signal_subgroup),
            tibble(signal_subgroup = str_trim(str_split(x$signal_subgroup, ",")[[1]]))
          )
        } else {
          x
        }
      })
  }

  dataframes$signals <- NULL
  dataframes <- map(dataframes, as_tibble)
  dataframes
}


#' normalize
#' @export normalize
normalize <- function(x) {
  x <- clean_names(x)
  x <- mutate_if(x, is.character, ~ str_squish(str_to_lower(.)))
  x
}
