
ui_header <- function(id = "header") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardHeader(skin = "dark")
}

ui_body <- function(id = "body") {
  box::use(shiny, bs4Dash)
  box::use(. / box / utilities / io / file_upload)
  ns <- shiny$NS(id)
  bs4Dash$dashboardBody(
    bs4Dash$tabItems(
      bs4Dash$tabItem(
        tabName = "tab1",
        file_upload$ui_file_upload()
      )
    )
  )
}

ui_sidebar <- function(id = "sidebar") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardSidebar(
    expandOnHover = FALSE,
    collapsed = TRUE,
    inputId = "sidebarState",
    bs4Dash$sidebarMenu(
      id = "sidebar",
      bs4Dash$menuItem(
        text = "Home",
        icon = shiny$icon("bars"),
        startExpanded = FALSE,
        bs4Dash$menuSubItem(
          text = "Item 1",
          tabName = "tab1",
          icon = shiny$icon("bars")
        ),
        bs4Dash$menuSubItem(
          text = "Item 2",
          tabName = "tab2",
          icon = shiny$icon("circle-thin")
        )
      )
    )
  )
}

ui_controlbar <- function(id = "controlbar") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardControlbar(
    collapsed = TRUE
  )
}

ui_footer <- function(id = "footer") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$bs4DashFooter()
}


ui_app <- function() {
  box::use(bs4Dash)
  bs4Dash$dashboardPage(
    header = ui_header(),
    body = ui_body(),
    sidebar = ui_sidebar(),
    controlbar = ui_controlbar(),
    footer = ui_footer()
  )
}

server_app <- function(session) {
  box::use(shiny, bs4Dash)
  box::use(. / box / utilities / io / file_upload)
  file_upload$server_file_upload(parentSession=session)
}

ui <- function() {
  ui_app()
}

server <- function(input, output, session) {
  server_app(session)
}

box::use(shiny)
shiny$shinyApp(ui, server)
