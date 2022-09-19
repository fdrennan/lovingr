#' @export analysis_missdiet
analysis_missdiet <- function(missdiet_data = NULL, variables) {
  print(missdiet_data)
  print(variables)
  box::use(dplyr, purrr)
  box::use(. / subfunction / compare_f)
  data_split <- split(missdiet_data, missdiet_data$paramcd)

  analysis_data <- purrr$map_df(
    data_split,
    function(x) {
      current_signal <- unique(x$paramcd)
      response <- compare_f$compare_f(
        r = x$incidence,
        n = x$numsubj,
        siteid = x$siteid,
        variables = variables
      )
      response$paramcd <- current_signal
      response
    }
  )

  # This exists because I want to touch CompareProportion as little as possible.
  analysis_data <-
    analysis_data |>
    dplyr$transmute(
      paramcd,
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
}
