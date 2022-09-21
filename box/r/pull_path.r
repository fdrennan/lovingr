#' pull_path
#'
#' @family csm_analysis_loop
#'
#' @export pull_path
pull_path <- function(mastersheet,
                      analysis_name) {
  analysis_path <- filter(mastersheet$data_paths, analysis == analysis_name)$final
  message(glue("Using path {analysis_path} for {analysis_name}."))
  analysis_path
}
