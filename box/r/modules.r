#' make_id
#' @export make_id
make_id <- function(data, label) {
  id <- unique(data$index)
  id <- glue("{label}{id}")
  id <- id[[1]]
  n <- nrow(data)
  label <- glue("{label} {n}")
  data <- map(data, unique)
  list(id = id, n = n, label = label, data = data)
}

#' cardUI
#' @export cardUI
cardUI <- function(data, label = "counter") {
  cli_alert_info("In cardUI")

  if (length(data) == 0) {
    return(NULL)
  }
  id_data <- make_id(data, label)
  ns <- NS(id_data$id)
  id <- id_data$id
  cli_alert_info("Building cardUI {id}")
  headers_data <- id_data$data[c("site", "country")]

  # Create potential issue, site, country labels for UI
  headers_html <- imap(headers_data, function(x, y) {
    cli_alert_info("Creating header header {y}")
    column(12, h2(to_title_case(y), ": ", toupper(x)))
  })

  data <- data %>% mutate_if(is.numeric, ~ round(., 1))
  data <- data %>% mutate(signal = str_remove(str_extract(unique(Summary.Statistics), "^.*:"), ":"))
  signal_data <- data %>%
    rowwise() %>%
    transmute(
      `Name` = name_of_endpoint_of_interest,
      Signal = paramcd_display,
      # Description = description,
      `N Subj` = n_subj,
      `N Subj Visit` = n_subj_visit,
      Site = glue(site_stat),
      Study = glue(study_stat),
      Plot = path_plot
    )


  signal_data <- remove_empty(signal_data, which = c("rows", "cols"))

  signal_data <- signal_data %>%
    mutate(
      Plot = if_else(is_file(Plot), make_hyperlink(paste0("file:///", Plot), "Open"), "")
    )

  signal_table <-
    tableHTML(mutate_if(signal_data, is.numeric, ~ round(., 2)),
      rownames = FALSE,
      class = "table1",
      escape = FALSE
    ) %>%
    add_css_row(
      css = list("background-color", "#f2f2f2"),
      rows = odd(1:nrow(signal_data))
    ) %>%
    add_css_row(
      css = list("background-color", "#e6f0ff"),
      rows = even(1:nrow(signal_data))
    )

  ui_card_html <-
    div(
      id = ns("cardshow"),
      fluidRow(
        class = "module",
        column(12, h1(to_title_case(unique(id_data$data$potential_issue)))),
        column(
          width = 4,
          fluidRow(
            headers_html,
            column(
              width = 12,
              selectizeInput(
                inputId = ns("action"),
                label = "Action",
                choices = c(
                  "Action Taken", "No Action Taken", "Not Useful"
                ),
                selected = if_else(unique(data$action) == "", "Action Taken", unique(data$action))
              )
            ),
            column(
              width = 12,
              textAreaInput(ns("comment"), "Comment", unique(data$comment), width = "100%", height = "100%")
            ),
            column(
              width = 12,
              actionButton(ns("button"), label = "Submit", class = "btn btn-primary btn-lg btn-block")
            )
          )
        ),
        column(width = 8, signal_table)
      )
    )

  ui_card_html
}

#' cardServer
#' @export cardServer
cardServer <- function(data, label = "counter", hide_card, out_path) {
  cli_alert_info("fn: cardServer")
  id_data <- make_id(data, label)
  id <- id_data$id
  cli_alert_info("Module id: {id}")

  mod_fun <- function(input, output, session) {
    cli_alert_info("fn: cardServer -> mod_fun")
    out <- reactiveValues()
    out$data <- data
    # dir_create(path_dir(unique(data$file_path)), recurse = TRUE)
    fp <- unique(data$file_path)
    dir_create(path_dir(fp), recurse = TRUE)
    observeEvent(input$button, {
      cli_alert_info("Button Pushed")
      out$data$comment <- input$comment
      out$data$action <- input$action
      cli_alert_info("hiding card {hide_card}")

      toggleClass("cardshow", "hide")
      # toggleClass("buttonshow", "show")

      cli_alert_info("Data stored at {fp}")

      write_rds(out$data, fp)
    })

    observeEvent(input$show, {
      toggleClass("cardshow", "show")
    })

    cli_alert_info("{input$comment}")
    cli_alert_info("{input$action}")

    # need to get input path here
    if (!"comment" %in% names(data)) {
      out$data$comment <- ""
      out$data$action <- ""
    }
    # data <- remove_empty(data, c('rows', 'cols'))
    # browsercli_alert_info("hiding card {hide_card}")()

    out$data <- out$data %>%
      distinct() %>%
      mutate(
        file_path = glue("output/app/{studyid}/{cutdt}-{site}-{country}-{scoreboard_group_id}.rds"),
        base_dir = path_dir(file_path)
      ) %>%
      select(
        studyid, cutdt, potential_issue, country, site, n_subj, n_subj_visit, paramcd_original, comment, action, index
      )



    write_rds(out$data, fp)
    out
  }

  moduleServer(
    id,
    mod_fun
  )
}

#' SUBSET_APPLICATION_DATA
#' @export SUBSET_APPLICATION_DATA
SUBSET_APPLICATION_DATA <- function(input_data) {
  group_index <-
    input_data %>%
    group_by(scoreboard_group_id, site) %>%
    group_indices()

  input_data$index <- with(input_data, paste0(site, study, group_index))
  input_data <- split(input_data, group_index)
  list(input_data)
}
