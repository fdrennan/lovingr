#' @export analysis_aegap
analysis_aegap <- function(aegap, configuration) {
  box::use(dplyr, tidyr)
  box::use(. / aegap / map_aegap_statistics)
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

  aegap_clean <- map_aegap_statistics$map_aegap_statistics(aegap_clean)

  aegap_clean
}


#' recode_binary_yes_no
#' @export recode_binary_yes_no
recode_binary_yes_no <- function(x) {
  box::use(dplyr[case_when])
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    TRUE ~ NA_real_
  )
}
