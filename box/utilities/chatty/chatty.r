#' @export
chatty <- function(session = shiny$getDefaultReactiveDomain, input, ...) {
  if (getOption("chatty")) {
    box::use(uuid, jsonlite, shiny, shinyToastify)
    json_data <- jsonlite$toJSON(shiny$reactiveValuesToList(input), pretty = TRUE)
    shinyToastify$showToast(
      session,
      input,
      id = uuid$UUIDgenerate(),
      shiny$fluidRow(
        shiny$column(
          12,
          shiny$pre(json_data)
        )
      ),
      style = list(
        border = "4px solid crimson",
        boxShadow = "rgba(0, 0, 0, 0.56) 0px 22px 30px 4px"
      ),
      bodyClassName = "row",
      toastClassName = "row",
      autoClose = FALSE
    )
  }
}
