#' @export analysis_aegap
analysis_aegap <- function(aegap, configuration) {
  box::use(dplyr, tidyr)
  box::use(. / aegap / map_aegap_statistics)
  aegap_clean <-
    aegap %>%
    dplyr$rename_all(str_to_lower) %>%
    tidyr$pivot_longer(
      names_to = "signals",
      values_to = "amount",
      cols = c(
        dplyr$contains("toxa"),
        dplyr$contains("numtox"),
        dplyr$contains("numae"),
        aegap,
        saegap
      )
    )

  aegap_clean <- dplyr$inner_join(aegap_clean, configuration$configuration)

  aegap_clean <- map_aegap_statistics$map_aegap_statistics(aegap_clean)

  aegap_clean
}


#' recode_binary_yes_no
#' @export recode_binary_yes_no
recode_binary_yes_no <- function(x) {
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    TRUE ~ NA_real_
  )
}



#' map_aegap_statistics
#' @export map_aegap_statistics
map_aegap_statistics <- function(df, configuration) {
  split_aegap <- split(df, df$signals)
  map(
    split_aegap,
    (
      run_aegap_analysis <- function(x) {
        mean_data <- x[!is.na(x$amount), ]

        mean_data$logAEGAP <- log(mean_data$amount)

        # drop the levels of SITEID without AEGAP ----
        mean_data$siteid <- factor(mean_data$siteid)

        rslt <- CompareMean(mean_data, "logAEGAP", "siteid", seed = 3)

        rslt$grp_mean <- exp(rslt$grp_mean)
        rslt$grandmean <- exp(rslt$grandmean)

        # diff between exp(grp_mean) and exp(grandmean) ----
        rslt$diff <- rslt$grp_mean - rslt$grandmean

        rslt <- rename(rslt, p_value = pvalue)

        rslt$cutdt <- unique(x$cutdt)
        rslt$code <- unique(x$code)
        rslt$signal <- unique(x$signals)
        rslt$paramcd <- unique(x$signals)
        rslt$site <- rslt$grp_id

        rslt
      })
  )
}
