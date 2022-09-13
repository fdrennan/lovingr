#' analysis_aegap
#'
#' @description
#'
#' # Columns available for flagging
#'
#'
#' ```
#' Columns: 16
#' $ grp_id    <chr> "0005"
#' $ n         <int> 8
#' $ grp_mean  <dbl> 347.8244
#' $ grandmean <dbl> 246.823
#' $ coef      <dbl> 0.3430263
#' $ sigma     <dbl> 0.2738144
#' $ tstat     <dbl> 1.252769
#' $ p_value   <dbl> 0.9944658
#' $ country   <chr> "USA"
#' $ diff      <dbl> 101.0014
#' $ cutdt     <dbl> 22314
#' $ code      <chr> "flag=0; if ((diff > 100) & (p_value<1) ) {flag = 1};"
#' $ signal    <chr> "aegap"
#' $ paramcd   <chr> "aegap"
#' $ site      <chr> "0005"
#' $ time      <dttm> 2021-03-25 03:44:43
#' ```
#'
#'
#' @param aegap aegap data
#' @param configuration data from mastersheet
#'
#' @return tibble
#' @family csm_analysis_loop
#' @family csm_analysis
#' @export analysis_aegap
analysis_aegap <- function(aegap, configuration) {
  # CompareMean -------------------------------------------------------------

  # this was added to R/
  # source("csm_dev/progclin/202102a_progclin/rfunction/CompareMean.r")

  # load data ---------------------------------------------------------------
  # aegap <- csm::csm_data_load(data_path = "csm_dev/datacollections/202102a_datamisc/csmae.sas7bdat")
  aegap_clean <-
    aegap %>%
    rename_all(str_to_lower) %>%
    mutate(
      aeyn = recode_binary_yes_no(aeyn),
      saeyn = recode_binary_yes_no(saeyn)
    ) %>%
    pivot_longer(
      names_to = "signals",
      values_to = "amount",
      cols = c(
        contains("toxa"),
        contains("numtox"),
        contains("numae"),
        aegap,
        saegap
      )
    )

  aegap_clean <- inner_join(aegap_clean, configuration$configuration)

  aegap_clean <- map_aegap_statistics(aegap_clean)

  aegap_clean <- map_df(
    aegap_clean,
    function(x) {
      map_df(
        split(x, 1:nrow(x)),
        flagger,
        analysis = "aegap"
      )
    }
  )

  aegap_clean$analysis <- "aegap"
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
