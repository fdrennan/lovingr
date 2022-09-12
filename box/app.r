#' @export
ui_app <- function() {
  box::use(
    bs4Dash,
    . / header / header,
    . / body / body,
    . / sidebar / sidebar,
    . / controlbar / controlbar,
    . / footer / footer,
  )
  bs4Dash$dashboardPage(
    header = header$ui_header(),
    body = body$ui_body(),
    sidebar = sidebar$ui_sidebar(),
    controlbar = controlbar$ui_controlbar(),
    footer = footer$ui_footer()
  )
}

#' @export
server_app <- function(session) {
  box::use(shiny, bs4Dash)
  box::use(
    bs4Dash,
    . / body / body
  )
  body$server_body(appSession = session)
}
