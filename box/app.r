#' @export
ui_app <- function() {
  box::use(
    bs4Dash, waiter,
    . / header / header,
    . / body / body,
    . / sidebar / sidebar,
    . / controlbar / controlbar,
    . / footer / footer
  )

  bs4Dash$dashboardPage(
    preloader = list(html = waiter$spin_1(), color = "#333e48"),
    title = "CSM Management",
    scrollToTop = TRUE,
    fullscreen = TRUE,
    header = header$ui_header(),
    body = body$ui_body(),
    sidebar = sidebar$ui_sidebar(),
    controlbar = controlbar$ui_controlbar(),
    footer = footer$ui_footer()
  )
}

#' @export
server_app <- function(session) {
  box::use(
    . / body / body,
    . / header / header,
    . / controlbar / controlbar
  )

  body$server_body(appSession = session)
  header$server_header()
  controlbar$server_controlbar()
}
