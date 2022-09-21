#' analysis_vitals;
#'
#' @description
#'
#' Here are the columns available for flagging
#'
#' ```
#' Columns: 12
#' $ count           <dbl> 5
#' $ nObs_Site       <dbl> 163
#' $ site_percentage <dbl> 3.067485
#' $ per_of_value    <dbl> 5.434783
#' $ Count_Study     <dbl> 92
#' $ nObs_Study      <dbl> 2670
#' $ PerofStudy      <dbl> 3.445693
#' $ odds_ratio      <dbl> 0.8802561
#' $ Pvalue          <dbl> 1
#' $ p_value         <dbl> 1
#' $ code            <chr> "flag=0; if (p_value < 0.1 & count > 8 & odds_ratio > 4 & site_percentage > 33 & per_of_value…
#' $ time            <dttm> 2021-03-25 03:51:12
#' ```
#' @family csm_analysis
#' @family csm_analysis_loop
#' @seealso rep_value_in_group
#' @export analysis_vitals
analysis_vitals <- function(input_vs = NULL, configuration = NULL) {
  split_vs <- input_vs %>%
    mutate(
      split_on = case_when(
        str_detect(paramcd, "dia") ~ paste0(vsdv, "_", paramcd),
        str_detect(paramcd, "sys") ~ paste0(vsdv, "_", paramcd),
        str_detect(paramcd, "bp") ~ paste0(vsdv, "_", paramcd),
        TRUE ~ paramcd
      )
    ) %>%
    split(.$split_on)

  flags <- imap_dfr(
    split_vs,
    ~ rep_value_in_group(..2, ..1)
  )

  csm_cli_header("VITAL SIGN ANALYSIS COMPLETE")

  join_meta_data <- distinct(input_vs, siteid, country, cutdt) %>%
    rename(Groups = siteid) %>%
    mutate(Groups = as.character(Groups))

  flags <- inner_join(flags, join_meta_data)

  flags <-
    flags %>%
    rename(
      site = Groups,
      value = Values
    )


  flags
}

#' rep_value_in_group
#'
#' @seealso analysis_vitals
#'
#' @export rep_value_in_group
rep_value_in_group <- function(signal_name = NULL, input_vs) {
  message(glue("Flagging {signal_name}"))
  signal_name_original <- signal_name
  if (str_detect(signal_name, "_")) {
    signal_name <- str_split(signal_name, "_")[[1]][[2]]
  }

  response <- suppressMessages({
    RepValueinGroup.f(
      signal_name,
      input_vs,
      "BY"
      # configuration = configuration
    )
  })

  response$signal_name <- signal_name_original

  response
}
