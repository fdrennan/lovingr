#' describe_package
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr select_if
#' @importFrom rlang .data
#' @importFrom utils installed.packages
#' @param package he name of a package
#'
#' @export describe_package
describe_package <- function(package = NULL) {
  packages <- as_tibble(installed.packages())
  pkg <- filter(packages, .data$Package == package)
  pkg <- select_if(pkg, function(x) {
    !all(is.na(x))
  })
  pkg <- t(pkg)

  return(pkg)
}
