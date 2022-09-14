#' @export compare_f
compare_f <- function(data = NULL, subfix = NULL, variables = NULL) {
  box::use(. / CompareProportion / CompareProportion)
  box::use(dplyr)
  # debug(CompareProportion$CompareProportion)
  #
  t_zscore_limit <- as.numeric(dplyr$filter(variables, parameter == "t_zscore")$value)
  min_n_value <- as.numeric(dplyr$filter(variables, parameter == "min_n_value")$value)
  min_n_number_betabinom <- as.numeric(
    dplyr$filter(variables, parameter == "min_n_number_betabinom")$value
  )

  aei_statistics <- CompareProportion$CompareProportion(
    r = data$incidence,
    n = data$numsubj,
    rowname = data$siteid,
    t_zscore_limit = t_zscore_limit,
    min_n_value = min_n_value,
    min_n_number_betabinom = min_n_number_betabinom
  )

  aei_statistics
}
