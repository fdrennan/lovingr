#' EXAMPLE_UPDATE_UI
#' @export EXAMPLE_UPDATE_UI
EXAMPLE_UPDATE_UI <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # about: Shiny app to demonstrate dynamic UI      #
  # author: Thomas FILAIRE                          #
  # date: 2020                                      #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

  # PART 1 - SETTING THE SCENE ----

  # section 1.0 - load required packages ----

  library(shiny) # web app framework for R
  library(shinythemes) # themes for shiny
  library(shinyWidgets) # custom input widgets for shiny
  library(tidyverse) # collection of R packages for data science

  # section 1.1 - build dataset ----

  crm_tbl <- tibble(
    character = c("Joyce Byers", "Jim Hopper"),
    actor = c("Winona Ryder", "David Harbour"),
    wiki = c("https://en.wikipedia.org/wiki/Winona_Ryder")
  )

  # section 1.2 - set initial selection ----

  current_user_selection <- "Jim Hopper"

  # section 1.3 - define custom infocard ----

  EXAMPLE_INFOCARD <- function(selected_character) {

    # selected_character == value from user pickerInput
    # e.g. "Mike Wheeler"

    selected_actor <- crm_tbl %>%
      filter(character == selected_character) %>%
      pull(actor) # get actor's name

    selected_wiki <- crm_tbl %>%
      filter(character == selected_character) %>%
      pull(wiki) # get wikipedia's link

    # piece of UI to render dynamically
    column(
      width = 4,
      div(
        class = "thumbnail text-center",
        # main information
        div(
          class = "caption",
          h4(selected_character),
          p(selected_actor)
        ),
        # link to wikipedia's page
        actionButton(
          class = "btn-default",
          inputId = str_glue("ab_more_{selected_character %>%
                                        tolower() %>%
                                        str_replace_all(' ', '_')}"),
          label = "More",
          onclick = str_glue("window.open('{selected_wiki}', '_blank')")
        ),
        # remove button
        actionButton(
          class = "btn-default rm_btn",
          inputId = str_glue("ab_remove_{selected_character %>%
                                        tolower() %>%
                                        str_replace_all(' ', '_')}"),
          label = "Remove"
        )
      )
    )
  }

  # PART 2 - UI PART ----
  ui <- navbarPage(
    title = "R Shiny advanced tips series",
    collapsible = TRUE,
    windowTitle = "R Shiny tips - TFI",
    theme = shinytheme("readable"), # use custom theme

    tabPanel(
      title = "Demo",

      # JS function to identify button id when clicked
      tags$head(
        tags$script(
          HTML("$(document).on('click', '.rm_btn', function () {
                                Shiny.onInputChange('rm_btn_id', this.id);
                             });")
        )
      ),

      # section 2.2 - sidebar panel ----
      div(
        class = "container",
        column(
          width = 3,
          wellPanel(
            div(
              shinyWidgets::pickerInput(
                inputId = "pi_character_selector",
                label = "Select character(s)",
                choices = crm_tbl %>% pull(character),
                multiple = FALSE,
                selected = "Mike Wheeler",
                options = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size = 10
                )
              )
            ),
            div(
              actionButton(
                class = "btn-primary",
                inputId = "ab_show_character",
                label = "Add"
              )
            )
          )
        ),
        column(
          width = 9,
          div(
            # render infocards dynamically (ui)
            uiOutput(outputId = "infocards")
          )
        )
      )
    )
  )

  # PART 3 - SERVER PART ----
  server <- function(input, output, session) {

    # get user's selected character when 'Add' button is clicked
    current_character <- eventReactive(
      eventExpr = input$ab_show_character,
      {
        input$pi_character_selector
      }
    )

    # store user's selection
    reactive_values <- reactiveValues()
    reactive_values$character_list <- current_user_selection

    # add character when relevant
    observeEvent(input$ab_show_character, {
      reactive_values$character_list <-
        c(
          reactive_values$character_list,
          current_character()
        ) %>%
        unique()
    })

    output$infocards <- renderUI({

      # render infocards dynamically (server)
      if (length(reactive_values$character_list) > 0) {
        reactive_values$character_list %>%
          map(EXAMPLE_INFOCARD) %>%
          tagList()
      }
    })

    # remove infocard
    observeEvent(input$rm_btn_id, {
      reactive_values$character_list <-
        reactive_values$character_list %>%
        .[reactive_values$character_list %>%
          toupper() != input$rm_btn_id %>%
          str_remove("ab_remove_") %>%
          toupper() %>%
          str_replace("_", " ")]
    })
  }

  # PART 4 - RUN APPLICATION ----
  shinyApp(ui = ui, server = server)
}
