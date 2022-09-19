#' @export
analysis_aegap <- function(aegap, variables) {
  box::use(. / analysis_aegap)
  box::use(purrr, dplyr)
  box::use(.. / ../ stats / CompareMean / CompareMean)
  df <- analysis_aegap$clean(aegap)
  split_aegap <- split(df, df$paramcd)
  purrr$map_dfr(
    split_aegap, (
      function(x) {
        tryCatch(
          {
            mean_data <- x[!is.na(x$amount), ]
            mean_data <- x[x$amount > 0, ] # added by freddy
            mean_data$logAEGAP <- log(mean_data$amount)
            #
            original_site_id <- mean_data$siteid
            mean_data$siteid <- factor(mean_data$siteid)

            rslt <- CompareMean$CompareMean(
              mean_data, "logAEGAP", "siteid",
              seed = 3
            )

            mean_data$siteid <- original_site_id

            rslt <-
              rslt |>
              dplyr$mutate(
                paramcd = unique(x$paramcd),
                grp_mean = exp(grp_mean),
                grandmean = exp(grandmean),
                diff_avg = grp_mean - grandmean
              ) |>
              dplyr$mutate(p_value = pvalue)

            rslt
          },
          error = function(err) {
            # shiny::showNotification(
            #   closeButton = TRUE, duration = NULL,
            #   shiny::div(
            #     shiny::tags$h5(paste0("aegap-", unique(x$paramcd))),
            #     shiny::tags$pre(as.character(err))
            #   )
            # )
            data.frame()
          }
        )
      })
  )
}




#' @export
clean <- function(aegap) {
  box::use(dplyr, tidyr)
  box::use(. / analysis_aegap)
  aegap_clean <-
    aegap |>
    dplyr$rename_all(tolower) |>
    dplyr$mutate(
      aeyn = analysis_aegap$recode_binary_yes_no(aeyn),
      saeyn = analysis_aegap$recode_binary_yes_no(saeyn)
    ) |>
    tidyr$pivot_longer(
      names_to = "paramcd",
      values_to = "amount",
      cols = c(
        dplyr$contains("toxa"),
        dplyr$contains("numtox"),
        dplyr$contains("numae"),
        aegap,
        saegap
      )
    )
}

#' recode_binary_yes_no
#' @export
recode_binary_yes_no <- function(x) {
  box::use(dplyr[case_when])
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    TRUE ~ NA_real_
  )
}
