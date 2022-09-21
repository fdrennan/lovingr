
#' BIND_DATA_AND_META
#' @export BIND_DATA_AND_META
BIND_DATA_AND_META <- function(x) {
  META <- x$META
  DATA <- x$DATA

  DATA$index <-
    x$DATA %>%
    group_by(potential_issue, site, cutdt, country) %>%
    group_indices() %>%
    as.character()

  DATA$path <- unique(META$path)

  META <-
    transmute(
      META,
      n_countries = n_distinct(DATA$country),
      n_site = n_distinct(DATA$site),
      n_paramcd = n_distinct(DATA$paramcd),
      cutdt = unique(DATA$cutdt),
      studyid = unique(DATA$studyid),
      path,
      scoreboard_button_name,
    )

  list(META = META, DATA = DATA)
}
