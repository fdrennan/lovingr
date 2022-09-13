
#' analysis_aecnt
#'
#' @param data Data derived from the data paths folder in CSM Mastersheet
#' @param configuration The configuration file, subsetted for aecnt.
#'
#' @description
#'
#' 'analysis_aecnt' has a slightly different process to complection
#' than the other analyses under the adverse event category. This is because
#' there is currently no R application for statistic generation - it is completed
#' in SAS.
#'
#' The SAS dataset is currently in a wide format, so in order to have the dynamic
#' selection of signals, we convert to a long format. This allows the application
#' to use the scoreboard more effectively.
#'
#' Columns available for flagging are listed below.
#'
#' ```
#' $ effect                 <chr> "siteid(country)"
#' $ siteid                 <int> 28
#' $ adjusted_p_value       <dbl> 1
#' $ method                 <chr> "anova"
#' $ estimate               <dbl> 0.04323285
#' $ avgcnt                 <dbl> 3.272727
#' $ n                      <int> 11
#' $ diff                   <dbl> -0.7788407
#' $ foldchange             <dbl> -1.237979
#' $ flag                   <int> 0
#' $ signals                <chr> "aecnt"
#' $ stdy_cnt               <dbl> 4.051568
#' $ site_cnt               <dbl> 3.272727
#' $ diff_cnt               <dbl> -0.7788407
#' $ study                  <dbl> 111302
#' $ category               <chr> "ae"
#' $ analysis               <chr> "aecnt"
#' ```
#' @family csm_analysis
#' @family csm_analysis_loop
#' @export analysis_aecnt
analysis_aecnt <- function(data, configuration) {
  split_names <- str_split(names(data), "_")
  types <- keep(split_names, ~ length(..1) > 1)
  types <- unique(map_chr(types, ~ ..1[[2]]))

  count_long <- map_dfr(
    types,
    function(signal) {
      signal_tag <- glue("_{signal}")
      df <- select(
        data, effect, siteid, ends_with(signal_tag)
      )
      names(df) <- str_remove_all(names(df), signal_tag)
      df$signals <- paste0(signal, "cnt")
      df
    }
  )

  count_long <- mutate(
    count_long,
    stdy_cnt = avgcnt - diff,
    site_cnt = avgcnt,
    diff_cnt = diff
  )

  count_long <- inner_join(count_long, configuration$configuration)

  # split_data <- split(count_long, 1:nrow(count_long))

  flags <- flagger(count_long, analysis = "aecnt")
  flags
}
