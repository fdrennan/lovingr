#' csm_util_rbind
#' @importFrom dplyr as_tibble
#' @param ... A list of dataframes
#'
#' @export csm_util_rbind
csm_util_rbind <- function(...) {
  as_tibble(bind_rows(...))
}
