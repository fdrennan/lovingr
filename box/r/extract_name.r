#' EXTRACT_NAME
#' @export EXTRACT_NAME
EXTRACT_NAME <- function(x) {
  last(str_split(x, "/")[[1]])
}
