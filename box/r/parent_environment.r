#' parent_environment
#' @param function_name
#' @export parent_environment
parent_environment <- function(function_name = NULL, function_description = NULL, main_level = 0, divlvl = 1) {
  if (!is.null(function_description)) {
    message(glue(function_description))
  }
  lvl <- sys.parent() - main_level
  message(glue("\nExecuting {function_name} | sys.parent {lvl}\n"))
  message(paste0(paste0(rep("==", trunc(lvl / divlvl)), collapse = ""), ">"))
  lvl
}
