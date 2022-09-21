#' build_scoreboard
#'
#' @description Build the scoreboard output from analysis specified in
#' `study_signal_generator()`.
#'
#'
#' @details `build_scoreboard()` takes two arguments (`analysis_results` and
#' `scoreboard`). The `analysis_resuls` is the list of analysis results, and
#' `scoreboard` contains the configuration parameters from the **Scoreboard**
#' sheet in **CSMMastersheet.xlsx**. More details are provided below:
#'
#'    1. Generate analysis results: the `study_signal_generator()` generates a
#'    list of all the analysis (signals) including (but not limited to):
#'    aecnt, aegap, aei, dosereason, overdose, underdose, vitals, rgv, rgm
#'
#'    2. The `analysis_results` is joined with the `scoreboard` object by the
#'    'analysis' and 'flag' columns (check the **CSMMastersheet.xlsx** file for
#'    more details).
#'
#'    3. All empty data.frames are removed from `analysis_results` (and an error
#'    is given if there are empty data.frames)
#'
#'    4. Each row is then split into it's own data.frame and passed off to
#'    `map_df()`, which attaches (!!) 'y' as a temporary variable to be used
#'    *inside* the glue for loop:
#'
#' ```
#' for (col in run_code) {
#'   y[col] <- glue(y[col][[1]])
#' }
#' detach(y)
#' y
#' ```
#'
#'
#' @export build_scoreboard
build_scoreboard <- function(analysis_results, scoreboard, n_sub_information, notify = TRUE) {
  message("Building scoreboard")

  analysis_results <- imap(analysis_results, function(x, y) {
    x$analysis <- y
    if (all(is.na(scoreboard$signal_subgroup))) {
      without_signal <- inner_join(x, filter(scoreboard, is.na(signal_subgroup)), by = c("analysis", "flag"))
      return(without_signal)
    } else {
      with_signal <- inner_join(x, rename(filter(scoreboard, !is.na(signal_subgroup)),
        paramcd = signal_subgroup
      ), by = c("flag", "paramcd", "analysis"))
      without_signal <- inner_join(x, filter(scoreboard, is.na(signal_subgroup)), by = c("analysis", "flag"))
    }

    bind_rows(with_signal, without_signal)
  })

  analysis_results <- APPLY_GROUP_FLAG(analysis_results)

  analysis_results <- keep(
    analysis_results,
    (nrow_more_than_zero <- function(x) {
      (nrow(x) > 0) & !("error" %in% colnames(x))
    })
  )

  analysis_results <-
    imap(
      analysis_results,
      function(x, y) {
        x$analysis <- y
        x
      }
    )

  # Split response tables and glue text -------------------------------------
  # we then take each row (now it's own data.frame) and pass it to map_df()
  analysis_results <- map(
    analysis_results,
    function(y) {
      try(expr = {
        # y <- split(y, 1:nrow(y))
        # browser()
        
        if ('siteid' %in% y) {
          cli_alert_danger('Need to update to not use, pick site only - clean up names related to site')
          y <- y %>% mutate(siteid = as.character(siteid))
        }
        y %>%
          mutate_if(is.numeric, ~ round(., 1)) %>%
          rowwise() %>%
          mutate(
            # siteid = as.character(siteid),
            site_stat = glue(site_stat),
            study_stat = glue(study_stat)
          ) %>%
          ungroup()
      })
    }
  )

  analysis_results <- analysis_results %>%
    keep(nrow_more_than_zero) %>%
    imap_dfr(format_scoreboard_loop) %>%
    mutate(cutdt = as.Date(cutdt, origin = "1960-1-1"))

  analysis_results <- analysis_results %>%
    mutate_if(is.character, str_squish) %>%
    left_join(n_sub_information)

  column_priority <-
    c(
      "time_run", "cutdt", "grp_id", "site", "Par", "country", "n",
      "r", "signals", "site_avg", "summary_level", "value", "n_subj",
      "stdy_avg", "site_pct", "ObsPer", "site_value_cnt", "n_subj_visit",
      "coef", "stdy_pct", "ExpPer", "nobs_site", "paramcd", "sigma",
      "diff_pct", "pvalue", "site_value_pct", "site_cnt", "tstat",
      "p_value", "pvalueMethod", "perofvalue", "diff_cnt", "method",
      "fence", "stdy_value_cnt", "foldchange", "diff_avg", "code",
      "cutoff_perplanned", "nobs_study", "adjusted_p_value", "flag",
      "stdy_value_pct", "signal", "study", "oddsratio", "category",
      "analysis", "flagging_specification", "studyid", "signal_name"
    )

  analysis_results <-
    select_at(analysis_results, vars(contains(column_priority)))

  # We want everything before we group by.
  analysis_results_raw <- analysis_results

  analysis_results <-
    analysis_results %>%
    group_by(potential_issue, site, scoreboard_group_id) %>%
    summarise(
      .groups = "keep",
      cutdt = paste0(unique(cutdt), collapse = ", "),
      site = paste0(unique(site), collapse = ", "),
      country = paste0(unique(country), collapse = ", "),
      n_subj = paste0(unique(n_subj), collapse = ", "),
      n_subj_visit = paste0(unique(n_subj_visit), collapse = ", "),
      paramcd = paste0(sort(unique(paramcd)), collapse = ","),
      description = paste0(sort(unique(description)), collapse = ", ")
    ) %>%
    arrange(potential_issue, site, paramcd) %>%
    mutate(description = str_replace_all(description, ", ", "\n")) %>%
    transmute(
      cutdt, potential_issue, country, site, n_subj, n_subj_visit,
      signals = paramcd, `Summary Statistics` = description
    ) %>%
    filter(!n_subj == "NA")

  out <- list(
    scoreboard = analysis_results,
    analysis_results = analysis_results_raw
  )
  out
}

#' apply_scoreboard
#' @export apply_scoreboard
apply_scoreboard <- function(x) {
  x <- split(x, 1:nrow(x))
  # we then take each row (now it's own data.frame) and pass it to map_df()

  map_df(
    x,
    apply_scoreboard_loop
  )
}

#' apply_scoreboard_loop
#' @export apply_scoreboard_loop
apply_scoreboard_loop <- function(y) {
  try(expr = {
    y %>%
      mutate_if(is.numeric, ~ round(., 1)) %>%
      rowwise() %>%
      mutate(site_stat = glue(site_stat), study_stat = glue(study_stat)) %>%
      ungroup()
  })
}
