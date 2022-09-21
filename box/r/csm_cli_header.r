#' csm_cli_header
#'
#' @param input_string To Write
#' @param width To Write
#'
#' @export csm_cli_header
csm_cli_header <- function(input_string, width = 0) {
  n_lines <- width - nchar(input_string)
  if (n_lines < 0) {
    n_lines <- 1
  }

  rhs_message <- paste0(rep("-", n_lines), collapse = "")
  message(paste0("- ", input_string, " ", rhs_message, collapse = ""))
}
