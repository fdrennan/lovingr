#' @export analysis_diet
analysis_diet <- function(aei_data = NULL, program = NULL, analysis = "diet") {
  parent_environment("analysis_aei")

  aei_data$paramcd <- "mdd"

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
