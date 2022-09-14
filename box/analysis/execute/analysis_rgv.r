#' @export analysis_rgv
analysis_rgv <- function(rgv_data = NULL, variables) {
  box::use(dplyr, purrr)
  box::use(. / subfunction / compare_f)
  box::use(. / subfunction / GVCheck.f / GVCheck.f)



  data <- GVCheck.f$GVCheck.f(
    rgv_data, "siteid", "paramcd", "numgv", "numrepgv", "GV",
    configuration = variables
  )



  # data_split <- split(rgv_data, rgv_data$paramcd)
  # browser()
  # analysis_data <- purrr$map_df(
  #   data_split,
  #   function(x) {
  #     current_signal <- unique(x$paramcd)
  #     response <- compare_f$compare_f(
  #       data = x,
  #       variables = variables
  #     )
  #   }
  # )
  data

  # analysis_data
}
