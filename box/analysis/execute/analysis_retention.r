#' analysis_retention
#'
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join
#'
#' @family analysis_ae
#'
#' @param retention_data A dataframe with analysis and signal columns
#' @param program To Write
#'
#' @description
#'
#' `analysis_retention` was one of the first analyses to be written. It is also the
#' cleanest of the analyses. We receive long format data, by which I mean
#' the data contains a column named analysis, and a column named paramcd or signal.
#' The alternative would be to receive a dataset with one column as analysis
#' and all subsequent columns named as n_signal_1, n_signal_2, etc. The latter
#' format is not preferred as it requires anticipation of column names instead
#' of creating a straightfoward split data.frame.
#'
#' # For examples of formatting, see `study_signal_generator`.
#'
#' # Available Columns for Flagging
#'
#' ```
#' $ site     <fct> 0005
#' $ r        <dbl> 5
#' $ n        <dbl> 8
#' $ n_subj   <dbl> 8
#' $ site_pct <dbl> 62.5
#' $ stdy_pct <dbl> 84.61538
#' $ p_value  <dbl> 0.8882447
#' $ method   <fct> Binom
#' $ diff_pct <dbl> -22.11538
#' ```
#'
#' The function inner joins the configuration file and `retention_data`.
#' For an example of an inner join, run the following
#'
#' ```
#' library(dplyr)
#' ```
#'
#' Once the join is complete, the retention_data is aplit on the param code. Each
#' param code data frame is then passed through ComparePropotion with
#' it's excel parameters to generate the statistics required for flagging.
#' This occurrs in compare_f. Finally, to edit CompareProportion as little as possible,
#' the data that is not passed into CompareProportion but is requested to remain
#' is joined back to the data as a final step.
#'
#'
#'
#' @family csm_analysis
#' @family csm_analysis_loop
#' @export analysis_retention
analysis_retention <- function(retention_data = NULL, program = NULL, analysis = "aei") {
  parent_environment("analysis_retention")
  #
  meta_data_join <- distinct(transmute(retention_data, site = siteid, country, cutdt))

  retention_data <- inner_join(
    retention_data,
    transmute(program$configuration, paramcd = signals, code)
  ) |>
    mutate(numsubj = ndenom)

  data_split <- split(retention_data, retention_data$paramcd)
  #
  analysis_data <- map_df(
    data_split,
    function(x) {
      tryCatch(expr = {
        current_signal <- unique(x$paramcd)
        message(glue("Executing signal generation for {current_signal}"))
        response <- compare_f(
          data = x,
          variables = program$parameters,
          analysis = analysis
        )
        response$paramcd <- current_signal
        response
      }, error = function(err) {
        distinct(x, paramcd, site = siteid, error = as.character(err))
      })
    }
  )

  #

  analysis_data <- inner_join(analysis_data, meta_data_join) |> mutate(analysis = "retention")
  analysis_data
}
