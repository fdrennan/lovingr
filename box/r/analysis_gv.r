#' analysis_gv
#' importFrom dplyr as_tibble
#' @param file_name Path to GV Data
#' @family csm_analysis_loop
#' @export analysis_gv
analysis_gv <- function(data = NULL, configuration = NULL) {
  data <- data %>% mutate(signals = tolower(as.character(param)))

  csm_cli_header("RUNNING BUILD GV ANALYSIS")

  # is_default <- all(!str_detect(configuration$configuration$signals, "select"))
  # if (is_default) {
  #   data <- bind_cols(data, select(configuration$configuration, -signals))
  # } else {
  data <- inner_join(data, configuration$configuration)
  # }
  data <- GVCheck.f(data, "siteid", "param", "n", "nrepgv", "GV", configuration = configuration, is_default = is_default)
  csm_cli_header("GV ANALYSIS COMPLETE")
  as_tibble(data)
}
#
# if (FALSE) {
#   library(testthat)
#   # library(csm)
#
#   home_path <- Sys.getenv("CSM_HOME")
#
#   csm_data_testing <- suppressWarnings({
#     analysis_gv(
#       file.path("csm_dev", "cache", "rinput302l.sas7bdat.rda")
#     )
#   })
#
#   test_that("build_gv_analysis works", {
#     expect_equal(csm_data_testing, csm_data_gv)
#   })
#
#   gv_output <- transmute(
#     .data = csm_data_testing,
#     site = rowname,
#     paramcd = GMParameter,
#     summary_level = "subj_visit",
#     r,
#     n_subj = 0,
#     n_subj_visit = n,
#     obs_pct = ObsPer,
#     stdy_pct = ExpPer,
#     diff_pct = obs_pct - stdy_pct,
#     p_value = pvalue,
#     method = pvalueMethod,
#     flag = 0
#   )
# }



# analysis site   paramcd summary_level  r n_subj n_subj_visit    obs_pct stdy_pct    diff_pct     p_value   method flag analysis site   paramcd summary_level  r n_subj n_subj_visit    obs_pct stdy_pct    diff_pct     p_value   method flag
