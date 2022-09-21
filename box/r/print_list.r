#' print_list
#' @export print_list
print_list <- function(list_data) {
  iwalk(list_data, function(x, y) {
    message(glue("Results for {y}"))
    glimpse(x)
    message("\n")
  })
}
