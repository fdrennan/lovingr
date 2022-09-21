# library(ANOM)
# Data
# Y is the name of outcome variable in Data
# Data should not have observations (rows) with missing Y and
# the each level of group_var should correspond to some non-missing Y


#' CompareMean
#'
#' @description
#'
#' Very little of CompareMean has changed, however there
#' is a renaming step within this function that I have added
#' to keep our output consistent with the preferred naming conventions.
#'
#'  On the left, we have the new namnes. On the right, the original.
#'
#'    grp_id = grps
#'    n = n
#'    grp_mean = grp_mean
#'    grandmean = grandmean
#'    coef = coefficients
#'    sigma = sigma
#'    tstat = tstat
#'    pvalue = pvalues
#'
#' @param Data A dataframe of signals
#' @param Y
#' @param group_var A signal name
#' @param seed Set seed
#'
#' @export CompareMean
CompareMean <- function(Data, Y, group_var, seed = 3) {

  # convert to data.frame for mean calculation
  Data <- as.data.frame(Data)

  set.seed(seed)

  grps <- levels(Data[, group_var])

  coef_name <- paste0(group_var, grps)

  n <- as.integer(table(Data[, group_var]))
  # print(all(names(table(MeanData[,group_var])) == grps))


  grandmean <- mean(Data[, Y])

  mod <- formula(paste0(Y, " ~ -1 + ", group_var))

  fit <- lm(mod, data = Data)
  grp_mean <- coef(fit)[coef_name]


  arg <- list("GrandMean")
  names(arg) <- group_var
  # print(names(arg))
  cmp <- do.call(mcp, arg)

  anom_test <- glht(fit, linfct = cmp, alternative = "two.sided")

  summ <- summary(anom_test)$test
  pvalues <- summ$pvalues

  coefficients <- summ$coefficients
  sigma <- summ$sigma
  tstat <- summ$tstat



  # diff <- grp_mean - grandmean
  country_map <-
    distinct(Data, siteid, country) %>%
    transmute(
      grp_id = siteid,
      country
    )
  rslt <- data.frame(
    grp_id = grps,
    n = n,
    grp_mean = grp_mean,
    grandmean = grandmean,
    coef = coefficients,
    sigma = sigma,
    tstat = tstat,
    pvalue = pvalues
  )

  rslt <- inner_join(rslt, country_map)

  return(rslt)
}
