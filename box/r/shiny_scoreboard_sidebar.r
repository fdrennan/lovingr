#' shiny_scoreboard_sidebar
#' @export shiny_scoreboard_sidebar
shiny_scoreboard_sidebar <- function(starting_data) {
  dashboardSidebar(
    selectizeInput("study", "Study",
      choices = unique(starting_data$studyid),
      starting_data$studyid[[1]]
    ),
    selectizeInput("cutdt", "Month",
      choices = unique(starting_data$cutdt),
      selected = starting_data$cutdt[[1]]
    ),
    checkboxInput(inputId = "show_all", label = "Hide After Submit", value = TRUE)
  )
}
