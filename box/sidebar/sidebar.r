#' @export
ui_sidebar <- function(id = "sidebar") {
  box::use(shiny, bs4Dash)
  ns <- shiny$NS(id)
  bs4Dash$dashboardSidebar(
    disable = TRUE,
    expandOnHover = FALSE,
    collapsed = TRUE,
    inputId = "sidebarState",
    bs4Dash$sidebarMenu(
      id = "sidebar",
      bs4Dash$menuItem(
        text = "Configuration",
        icon = shiny$icon("bars"),
        startExpanded = FALSE,
        bs4Dash$menuSubItem(
          text = "Options",
          tabName = "tab0",
          icon = shiny$icon("cog")
        ),
        bs4Dash$menuSubItem(
          text = "Upload",
          tabName = "tab1",
          icon = shiny$icon("bars")
        ),
        bs4Dash$menuSubItem(
          text = "Raw Input",
          tabName = "tab2",
          icon = shiny$icon("circle-thin")
        )
      )
    )
  )
}
