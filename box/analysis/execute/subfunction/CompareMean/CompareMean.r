#' @export CompareMean
CompareMean <- function(Data, Y, group_var, seed = 3) {
  box::use(multcomp)
  # convert to data.frame for mean calculation
  Data <- as.data.frame(Data)

  set.seed(seed)

  grp_id <- levels(Data[, group_var])

  coef_name <- paste0(group_var, grp_id)

  n <- as.integer(table(Data[, group_var]))

  grandmean <- mean(Data[, Y])

  mod <- formula(paste0(Y, " ~ -1 + ", group_var))

  fit <- lm(mod, data = Data)
  grp_mean <- coef(fit)[coef_name]


  arg <- list("GrandMean")
  names(arg) <- group_var
  cmp <- do.call(mcp, arg)

  anom_test <- multcomp$glht(fit, linfct = cmp, alternative = "two.sided")

  summ <- summary(anom_test)$test

  country_map <- dplyr$distinct(Data, siteid, country)

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

  return(rslt)
}
