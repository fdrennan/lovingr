#' pull_filter_data
#' @export pull_filter_data
pull_filter_data <- function(input) {
  cli_alert_info("Pulling data for {input$study} at {input$cutdt}")
  if (input$study == "" | input$cutdt == "") {
    cli_alert_warning("Query parameters not complete")
    return(list())
  }
  input_data <- input_data_all %>%
    keep(function(x) {
      input$study == x[[1]]$studyid[[1]] & input$cutdt == x[[1]]$cutdt[[1]]
    })

  input_data <- input_data[[1]]
  input_data
}
