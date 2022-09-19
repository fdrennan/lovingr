#' #' @export
#' chatty <- function(session = shiny$getDefaultReactiveDomain, input, ...) {
#'   if (getOption("chatty")) {
#'     box::use(uuid, jsonlite, shiny, shinyToastify)
#'     json_data <- jsonlite$toJSON(shiny$reactiveValuesToList(input), pretty = TRUE)
#'     shinyToastify$showToast(
#'       session,
#'       input,
#'       id = uuid$UUIDgenerate(),
#'       shiny$fluidRow(
#'         shiny$column(
#'           12,
#'           shiny$pre(json_data)
#'         )
#'       ),
#'       # toastClassName = "chatty",
#'       # bodyClassName = "chatty",
#'       className = "chatty",
#'       position = "top-center",
#'       autoClose = 2000
#'     )
#'   }
#' }
