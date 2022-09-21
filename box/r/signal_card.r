#' SIGNAL_CARD
#' @export SIGNAL_CARD
#' @description the SIGNAL_CARD/SIGNAL_CARD_LOOP functions are used to create
SIGNAL_CARD <- function(x) {
  fluidRow({
    map(x, SIGNAL_CARD_LOOP)
  })
}


# SIGNAL_CARD_LOOP --------------------------------------------------------

SIGNAL_CARD_LOOP <- function(x) {
  ## values for signal card  ----
  potential_issue <- unique(x$potential_issue)
  site <- unique(x$site)
  country <- unique(x$country)

  # id ----------------------------------------------------------------------
  id <- paste0("i", unique(x$index), "i")
  action_taken <- paste0("action_", unique(x$index), "_taken")

  cli_alert_info(text = "{id} card")

  NAME <- if_else(is.null(x$name[[1]]), "", x$name[[1]])

  # hides card with resolved issues (stored in NAME)
  STYLE <- if_else(NAME == "", "border: 1px solid; margin: 10px", "display: none;")

  ############ CARD FORMAT
  column(
    width = 12,
    style = STYLE,
    fluidRow(
      column(
        width = 2,
        tags$p(tags$b("Potential Issue: "), potential_issue),
        tags$p(tags$b("Site: "), site),
        tags$p(tags$b("Country: "), country)
      ),
      column(width = 3, map(x$description, tags$p)),
      column(
        width = 2,
        selectizeInput(
          inputId = action_taken,
          label = "Action",
          choices = c(
            "Action 1" = "axn_01",
            "Action 2" = "axn_02",
            "Action 3" = "axn_03"
          ),
          multiple = FALSE,
          options = list(create = TRUE)
        )
      ),
      column(
        width = 4,
        textInput(inputId = id, "Comment", NAME),
        # submit button (submit_action) ----
        # actionButton(inputId = "submit_action",
        # label = "Submit",
        # class = "btn-success"))
      )
    )
  )
  ############
}
