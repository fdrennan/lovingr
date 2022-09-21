
#' make_configuration
#' @family csm_analysis_loop
#' @export make_configuration
make_configuration <- function(mastersheet, function_names, analysis_name) {
  list(
    parameters = filter(
      mastersheet$analysis_variables,
      function_name %in% function_names
    ),
    configuration = filter(
      .data = mastersheet$flagging_setup,
      .data$analysis == analysis_name
    )
  )
}
