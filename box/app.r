#' @export
ui_app <- function() {
  box::use(
    bs4Dash,
    . / header / header,
    . / body / body,
    . / sidebar / sidebar,
    . / controlbar / controlbar,
    . / footer / footer
  )



  bs4Dash$dashboardPage(
    scrollToTop = TRUE,
    title = "CSM Management",
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
    . / controlbar / controlbar
  )

  body$server_body(appSession = session)
  controlbar$server_controlbar()
}
