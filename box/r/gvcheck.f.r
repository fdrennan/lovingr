#' GVCheck.f
#'
#' @param GVData To Write
#' @param Var_site To Write
#' @param Var_by To Write
#' @param Var_n To Write
#' @param Var_r To Write
#' @param name To Write
#'
#' @importFrom stats aggregate
#'
#' @export GVCheck.f
GVCheck.f <- function(GVData, Var_site, Var_by, Var_n, Var_r, name, configuration = NULL, is_default = FALSE) {
  T_ZSCORE <- as.numeric(filter(configuration$parameters, parameter == "t_zscore")$value)
  # This analysis is based on the incidence of repeated values across the site,
  # ignoring the subject level structure. Futhermore, the analysis is done with all parameters
  # combined, and also separated.

  # Analysis based on data combined all the Growth parameters in the analysis;

  # aggreate r and n across subjects and parameters;
  DataComb <- aggregate(GVData[, c(Var_r, Var_n)], by = list(GVData[, Var_site])[[1]], sum)
  names(DataComb)[1] <- Var_site

  ResComb <- CompareProportion(as.numeric(DataComb[, Var_r]), as.numeric(DataComb[, Var_n]), DataComb[, Var_site], T_ZSCORE)

  GMParameter <- rep("Combined", dim(ResComb)[1])
  ResComb <- data.frame(GMParameter, ResComb)

  result <- ResComb
  # aggreate r and n across subjects for each parameter;
  DatabyPar <- aggregate(GVData[, c(Var_r, Var_n)], by = map_dfc(list(GVData[, Var_site], GVData[, Var_by]), ~.), sum)
  names(DatabyPar)[1:2] <- c(Var_site, Var_by)

  parm <- as.character(unique(DatabyPar[, Var_by]))
  nparm <- length(parm)


  for (i in 1:nparm)
  {
    parname <- parm[i]
    index <- (as.character(DatabyPar[, Var_by]) == parname)
    ResI <- CompareProportion(as.numeric(DatabyPar[index, Var_r]), as.numeric(DatabyPar[index, Var_n]), DatabyPar[index, Var_site], T_ZSCORE)
    GMParameter <- rep(parname, dim(ResI)[1])
    ResI <- data.frame(GMParameter, ResI)
    result <- rbind(result, ResI)
  }

  GVmethod <- rep(name, dim(result)[1])


  # diff <- result$ObsPer - result$ExpPer
  # Flag <- (diff > 10 & result$pvalue < 0.05 & result$ObsPer >= 10 & result$r >= 3) + 0

  # flag_aei_data

  # result <-
  #   result %>%
  #   mutate(
  #     site_percentage = ObsPer
  #   )
  result <-
    transmute(
      .data = result,
      cutdt = GVData$cutdt[[1]],
      site = rowname,
      signals = GMParameter,
      summary_level = "subj_visit",
      r,
      n,
      site_pct = ObsPer,
      stdy_pct = ExpPer,
      diff_pct = site_pct - stdy_pct,
      p_value = pvalue,
      method = pvalueMethod
    )

  result <- inner_join(result, configuration$configuration)

  result <- flagger(result, "rgv")

  return(result)
}
