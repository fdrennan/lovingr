#' map_aegap_statistics
#' @export map_aegap_statistics
map_aegap_statistics <- function(df, configuration) {
  box::use(purrr, dplyr)
  box::use(.. / subfunction / CompareMean / CompareMean)
  split_aegap <- split(df, df$signals)
  purrr$map(
    split_aegap,
    (
      run_aegap_analysis <- function(x) {
        mean_data <- x[!is.na(x$amount), ]

        mean_data$logAEGAP <- log(mean_data$amount)

        # drop the levels of SITEID without AEGAP ----
        mean_data$siteid <- factor(mean_data$siteid)

        rslt <- CompareMean$CompareMean(
          mean_data, "logAEGAP", "siteid",
          seed = 3
        )

        rslt$grp_mean <- exp(rslt$grp_mean)
        rslt$grandmean <- exp(rslt$grandmean)

        # diff between exp(grp_mean) and exp(grandmean) ----
        rslt$diff <- rslt$grp_mean - rslt$grandmean

        rslt <- dplyr$rename(rslt, p_value = pvalue)

        rslt$cutdt <- unique(x$cutdt)
        rslt$code <- unique(x$code)
        rslt$signal <- unique(x$signals)
        rslt$paramcd <- unique(x$signals)
        rslt$site <- rslt$grp_id

        rslt
      })
  )
}
