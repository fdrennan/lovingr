#' format_scoreboard_loop
#' @export format_scoreboard_loop
format_scoreboard_loop <- function(x, y) {
  if (y == "vitals") {
    x <- mutate(x, paramcd = signal_name)
  } else {
    x$value <- NA
  }

  x <-
    mutate(
      .data = x,
      paramcd_original = paramcd,
      paramcd = case_when(
        analysis == "vitals" ~ glue("{signal_prefix} {paramcd} ({value})"),
        TRUE ~ glue("{signal_prefix} {paramcd}")
      ),
      paramcd_display = paramcd,
      description = case_when(
        TRUE ~ glue("{paramcd} - {name_of_endpoint_of_interest}: site = {site_stat} vs. study = {study_stat}")
      )
    )

  x <-
    x %>%
    group_by(potential_issue, site, scoreboard_group_id) %>%
    mutate(paramcd = paste0(unique(paramcd), collapse = ", ")) %>%
    ungroup()

  x <- split(x, paste0(x$potential_issue, x$site, x$scoreboard_group_id))

  x <- map_df(x, function(y) {
    max_signal_names <- unique(y$max_number_signal_summary)
    print(max_signal_names)
    site_diff <- unique(y$sitediff_vs_study)
    if (sign(max_signal_names) == 1) {
      y <- arrange_at(y, site_diff, list(desc = desc))
    } else {
      y <- arrange_at(y, site_diff)
    }
    # print(max_signal_names)
    head(y, abs(max_signal_names))
  })

  x
}


#' format_scoreboard
#' @export format_scoreboard
format_scoreboard <- function(sb) {
  csm_cli_header("Formatting")

  # split on potential issue and analysis
  sb <- imap_dfr(sb, format_scoreboard_loop)

  sb <- mutate(.data = sb, cutdt = as.Date(cutdt, origin = "1960-1-1"))

  sb
}
