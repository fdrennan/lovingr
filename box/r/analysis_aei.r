#' analysis_aei
#'
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join
#'
#' @family analysis_ae
#'
#' @param aei_data A dataframe with analysis and signal columns
#' @param program To Write
#'
#' @description
#'
#' `analysis_aei` was one of the first analyses to be written. It is also the
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
#' The function inner joins the configuration file and `aei_data`.
#' For an example of an inner join, run the following
#'
#' ```
#' library(dplyr)
#' band_members %>% inner_join(band_instruments)
#' ```
#'
#' Once the join is complete, the aei_data is aplit on the param code. Each
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
#' @export analysis_aei
analysis_aei <- function(aei_data = NULL, program = NULL, analysis = "aei") {
  parent_environment("analysis_aei")

  aei_data <- inner_join(
    aei_data,
    transmute(program$configuration, paramcd = signals, code)
  )

  data_split <- split(aei_data, aei_data$paramcd)

  analysis_data <- map_df(
    data_split,
    function(x) {
      current_signal <- unique(x$paramcd)
      message(glue("Executing signal generation for {current_signal}"))
      response <- compare_f(
        data = x,
        variables = program$parameters,
        analysis = analysis
      )
      response$paramcd <- current_signal
      response
    }
  ) %>% as_tibble()

  aei_data <- mutate(aei_data, site = siteid, country, cutdt)
  meta_data_join <- distinct(aei_data, site, country, cutdt)
  analysis_data <- inner_join(analysis_data, aei_data)

  analysis_data
}


#' analysis_aei
#'
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join
#'
#' @family analysis_ae
#'
#' @param aei_data A dataframe with analysis and signal columns
#' @param program To Write
#'
#' @description
#'
#' `analysis_aei` was one of the first analyses to be written. It is also the
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
#' The function inner joins the configuration file and `aei_data`.
#' For an example of an inner join, run the following
#'
#' ```
#' library(dplyr)
#' band_members %>% inner_join(band_instruments)
#' ```
#'
#' Once the join is complete, the aei_data is aplit on the param code. Each
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
#' @export analysis_diet
analysis_diet <- function(aei_data = NULL, program = NULL, analysis = "diet") {
  parent_environment("analysis_aei")

  aei_data <- inner_join(
    aei_data,
    transmute(program$configuration, paramcd = signals, code)
  )

  data_split <- split(aei_data, aei_data$paramcd)

  analysis_data <- map_df(
    data_split,
    function(x) {
      current_signal <- unique(x$paramcd)
      message(glue("Executing signal generation for {current_signal}"))
      response <- compare_f(
        data = x,
        variables = program$parameters,
        analysis = analysis
      )
      response$paramcd <- current_signal
      response
    }
  )

  if (!is.null(aei_data$cutdt[[1]])) {
    analysis_data$cutdt <- aei_data$cutdt[[1]]
  } else {
    analysis_data$cutdt <- "2999-01-01"
  }

  analysis_data
}


#' @family csm_analysis
#' @family csm_analysis_loop
#' @export analysis_misseddose
analysis_misseddose <- function(aei_data = NULL, program = NULL, analysis = "misseddose") {
  parent_environment("analysis_aei")

  aei_data <- inner_join(
    aei_data,
    transmute(program$configuration, paramcd = signals, code)
  )

  data_split <- split(aei_data, aei_data$paramcd)

  analysis_data <- map_df(
    data_split,
    function(x) {
      current_signal <- unique(x$paramcd)
      message(glue("Executing signal generation for {current_signal}"))
      response <- compare_f(
        data = x,
        variables = program$parameters,
        analysis = analysis
      )
      response$paramcd <- current_signal
      response
    }
  )

  if (!is.null(aei_data$cutdt[[1]])) {
    analysis_data$cutdt <- aei_data$cutdt[[1]]
  } else {
    analysis_data$cutdt <- "2999-01-01"
  }

  analysis_data
}
