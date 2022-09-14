#' @export analysis_aecnt
analysis_aecnt <- function(data, configuration) {
  box::use(purrr, stringr, dplyr, glue)
  split_names <- stringr$str_split(names(data), "_")
  types <- purrr$keep(split_names, ~ length(..1) > 1)
  types <- unique(purrr$map_chr(types, ~ ..1[[2]]))

  count_long <- purrr$map_dfr(
    types,
    function(signal) {
      signal_tag <- glue$glue("_{signal}")
      df <- dplyr$select(
        data, effect, siteid, dplyr$ends_with(signal_tag)
      )
      names(df) <- stringr$str_remove_all(names(df), signal_tag)
      df$signals <- paste0(signal, "cnt")
      df
    }
  )

  count_long <- dplyr$mutate(
    count_long,
    stdy_cnt = avgcnt - diff,
    site_cnt = avgcnt,
    diff_cnt = diff
  )

  count_long
}
