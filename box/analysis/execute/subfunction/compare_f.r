#' @export compare_f
compare_f <- function(data = NULL, subfix = NULL, variables = NULL, analysis = "aei") {
  
  aei_statistics <- CompareProportion(
    r = data$incidence,
    n = data$numsubj,
    rowname = data$siteid,
    t_zscore_limit = as.numeric(filter(variables, parameter == "t_zscore")$value),
    min_n_value = as.numeric(filter(variables, parameter == "min_n_value")$value),
    min_n_number_betabinom = as.numeric(filter(variables, parameter == "min_n_number_betabinom")$value)
  )
  
  # This exists because I want to touch CompareProportion as little as possible.
  aei_statistics <- transmute(
    aei_statistics,
    site = .data$rowname,
    r = .data$r,
    n = .data$n,
    n_subj = .data$n,
    site_pct = ObsPer,
    stdy_pct = ExpPer,
    diff_pct = site_pct - stdy_pct,
    p_value = .data$pvalue,
    method = pvalueMethod,
    stdy_r,
    stdy_n
  )
  
  aei_statistics$code <- unique(data$code)
  # split_data <- split(aei_statistics, 1:nrow(aei_statistics))
  # browser()
  flags <- flagger(aei_statistics, analysis = "aei")
  
  csm_cli_header("Flagging applied ")
  
  flags
}
