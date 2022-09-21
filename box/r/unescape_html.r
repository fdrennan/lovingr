#' unescape_html
#' @importFrom xml2 xml_text
#' @importFrom xml2 read_html
#' @param str To Write
#'
#' @export unescape_html
unescape_html <- function(str) {
  xml_text(read_html(paste0("<x>", str, "</x>")))
}
