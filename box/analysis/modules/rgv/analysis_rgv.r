#' @export analysis_rgv
analysis_rgv <- function(rgv_data = NULL, variables) {
  box::use(dplyr, purrr)
  box::use(.. / .. / stats / CompareProportion / compare_f)
  box::use(.. / .. / stats / GVCheck.f / GVCheck.f)

  data <- GVCheck.f$GVCheck.f(
    rgv_data, "siteid", "paramcd", "numgv", "numrepgv", "GV",
    configuration = variables
  )
}
