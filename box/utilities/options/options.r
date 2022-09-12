#' @export
ui_options <- function(id = "options") {
  box::use(shiny, shinyWidgets)
  ns <- shiny$NS(id)
  shinyWidgets$prettyToggle(
    ns("development"),
    label_on = "Development",
    label_off = "Production",
    value = TRUE
  )
}

#' @export
server_options <- function(id = "options") {
  box::use(shiny, glue)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      shiny$observeEvent(input$development, {
        options(development = input$development)
      })

      output$ui <- shiny$renderUI({
        # options(shiny.maxRequestSize = 300 * 1024^2)
        # options(development = TRUE)
        # options(base_config = "Config.xlsx")
        # options(cache = getOption("development"))
        # options(file_regex = "csm[0-9]{6}[a|b|c]/datamisc$")
        # options(datamisc_cache_path = "./datamisc")
        # options(bmrn_base_dir = "/sassys/cdm/cdmdev/bmn111/ach")
        # options(base_directory = getOption("bmrn_base_dir"))
        # options(cache_path = "./cache/data.rda")
        # options(analysis_filter = {
        #   if (getOption("development")) {
        #     c("aei", "rgv", "vitals", "rgm", "underdose", "aegap", "aecnt")
        #   } else {
        #     c("aei", "rgv", "vitals", "rgm", "underdose", "aegap", "aecnt")
        #   }
        # })
      })

      input
    }
  )
}
