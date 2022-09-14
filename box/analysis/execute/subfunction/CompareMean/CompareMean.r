#' @export CompareMean
CompareMean <- function(data, Y, group_var, seed = 3) {
  box::use(multcomp, dplyr, stats)
  data <- as.data.frame(data)

  set.seed(seed)

  siteid <- levels(data[, group_var])

  coef_name <- paste0(group_var, siteid)

  n <- as.integer(table(data[, group_var]))

  grandmean <- mean(data[, Y])
  mod <- stats$formula(paste0(Y, " ~ -1 + ", group_var))

  # dplyr$as_tibble(data)
  fit <- stats$lm(mod, data = data)
  grp_mean <- stats$coef(fit)[coef_name]


  arg <- list("GrandMean")
  names(arg) <- group_var
  cmp <- do.call(multcomp$mcp, arg)

  anom_test <- multcomp$glht(fit, linfct = cmp, alternative = "two.sided")

  summ <- summary(anom_test)$test

  country_map <- dplyr$distinct(data, siteid, country)

  rslt <- data.frame(
    siteid = siteid,
    n = n,
    grp_mean = grp_mean,
    grandmean = grandmean,
    coef = summ$coefficients,
    sigma = summ$sigma,
    tstat = summ$tstat,
    pvalue = summ$pvalues
  )

  rslt <- dplyr$inner_join(rslt, country_map)

  rslt
}
