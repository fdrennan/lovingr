#' @export
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
