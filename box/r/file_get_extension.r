#' get_extension
#' @param file_path A string
#' @export file_get_extension
file_get_extension <- function(file_path) {
  # split the file and extension part into two elements
  ex <- strsplit(basename(file_path), split = "\\.")[[1]]
  # return the extension
  return(ex[length(ex)])
}
