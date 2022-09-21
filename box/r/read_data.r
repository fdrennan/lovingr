#' READ_DATA
#' @export READ_DATA
READ_DATA <- function(scoreboard) {
  signals <- split(scoreboard, with(scoreboard, paste0(potential_issue, site, country, paramcd)))
  summary_data <- map2(signals, 1:length(signals), function(x, y) {
    x <- SIGNAL_META(x)
    x$index <- y
    x
  })
  summary_data
}
