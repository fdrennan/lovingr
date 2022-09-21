#' #' shiny_starting_data
#' #' @export shiny_starting_data
#' shiny_starting_data <- function(DIRECTORY_BASE) {
#'
#'   plot_locations <- pull_plot_locations(DIRECTORY_BASE)
#'   starting_data$input_data_all[[1]] <- left_join(
#'     bind_rows(starting_data$input_data_all[[1]]),
#'     plot_locations
#'   )
#'
#'   list(starting_data = starting_data, plot_locations = plot_locations)
#' }
