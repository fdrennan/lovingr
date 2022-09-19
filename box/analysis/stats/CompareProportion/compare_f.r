#' @export compare_f
compare_f <- function(r, n, siteid = NULL, subfix = NULL, variables = NULL) {
  box::use(. / CompareProportion)
  box::use(dplyr)
  t_zscore_limit <- as.numeric(dplyr$filter(variables, parameter == "t_zscore")$value)
  min_n_value <- as.numeric(dplyr$filter(variables, parameter == "min_n_value")$value)
  min_n_number_betabinom <- as.numeric(
    dplyr$filter(variables, parameter == "min_n_number_betabinom")$value
  )

  aei_statistics <- CompareProportion$CompareProportion(
    r = r,
    n = n,
    rowname = siteid,
    t_zscore_limit = t_zscore_limit,
    min_n_value = min_n_value,
    min_n_number_betabinom = min_n_number_betabinom
  )
}
