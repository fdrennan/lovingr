#' map_aegap_statistics
#' @export map_aegap_statistics
map_aegap_statistics <- function(df, configuration) {
  box::use(purrr, dplyr)
  box::use(.. / subfunction / CompareMean / CompareMean)
  split_aegap <- split(df, df$paramcd)
  purrr$map_dfr(
    split_aegap, (
      function(x) {
        tryCatch(
          {
            # browser()
            mean_data <- x[!is.na(x$amount), ]
            mean_data <- x[x$amount > 0, ] # added by freddy
            mean_data$logAEGAP <- log(mean_data$amount)
            mean_data$siteid <- factor(mean_data$siteid)

            rslt <- CompareMean$CompareMean(
              mean_data, "logAEGAP", "siteid",
              seed = 3
            )

            rslt <-
              rslt |>
              dplyr$mutate(
                grp_mean = exp(grp_mean),
                grandmean = exp(grandmean),
                diff = grp_mean - grandmean,
                paramcd = unique(x$paramcd)
              )

            rslt
          },
          error = function(err) {
            shiny::showNotification(
              shiny::div(
                shiny::tags$h5(unique(x$paramcd)),
                shiny::tags$p(as.character(err))
              )
            )
            data.frame()
          }
        )
      })
  )
}
