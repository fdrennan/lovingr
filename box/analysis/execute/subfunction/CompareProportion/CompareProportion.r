#' @export CompareProportion
CompareProportion <- function(r, n, rowname, t_zscore_limit = 1.69, min_n_value = 2, min_n_number_betabinom = 5) {
  
  # If Sys.getenv('t_zscore') is equal to "" then t_zscore_limit will be used.
  t_zscore_limit <- as.numeric(set_env("t_zscore", last(t_zscore_limit)))
  min_n_value <- as.numeric(set_env("min_n_value", min_n_value))
  min_n_number_betabinom <- as.numeric(set_env("min_n_number_betabinom", min_n_number_betabinom))
  
  csm_cli_header("Running CompareProportion")
  # added 8/20/2019: to remove the NAs in r and n, due to some sites missing a particular measurement;
  r <- r[!is.na(r + n)]
  n <- n[!is.na(r + n)]
  
  # added 07/01/2020: only keep sites whose n >= 2
  min_n_to_keep <- which(n >= min_n_value)
  r <- r[min_n_to_keep]
  n <- n[min_n_to_keep]
  
  # create rownames from sites with n > 2
  rowname <- rowname[min_n_to_keep]
  
  ObsPer <- 100 * r / n
  N <- length(r) # number of site, e.g.;
  
  # binomial probability;
  ExpPer_Binom <- 100 * sum(r) / sum(n)
  # stdy_r = sum(r), stdy_n = sum(n) / For all compare proportion
  ### New change 05/21/2020
  if (ExpPer_Binom == 0 | ExpPer_Binom == 100) {
    # potential change from Derek on 2021-05-12
    # if (ExpPer_Binom == 0 | ExpPer_Binom == 100 | is.na(ExpPer_Binom)) {
    ObsPer <- ExpPer_Binom
    ExpPer <- ExpPer_Binom
    pvalue <- 1
    pvalueMethod <- "No analysis"
  } ### New change 05/21/2020
  else {
    pbinom <- 1 - pbinom(r, n, ExpPer_Binom / 100)
    
    # when r=n we have issue;
    
    # betabinomial probability
    fit <- vglm(cbind(r, n - r) ~ 1, betabinomial, trace = FALSE)
    Coef <- Coef(fit)
    
    # print(Coef)
    
    ExpPer_Betabinom <- 100 * Coef[1]
    pbetabinom <- 1 - pbetabinom(r, n, prob = ExpPer_Betabinom / 100, rho = Coef[2], log.p = FALSE)
    
    # Add the CVs code from updated CompareProportion.R
    # 01/26/2021 calculate standard error/abs(estimate) for each parameter.
    # monitoring if the variance of estimators are too large
    CVs <- summaryvglm(fit)@coef3[, 2] / abs(summaryvglm(fit)@coef3[, 1])
    
    T_Zscore <- Tarone.test(r, n)
    
    # improvment can be made to include a goodness fit for betabinomial modeling, if it fails, we will use bionomial test; but for now only use Taronetest;
    
    message(glue("\tT_Zscore={round(T_Zscore, 3)}\nt_zscore_limit={t_zscore_limit}"))
    # if ((T_Zscore > t_zscore_limit) & all(CVs < 5)) {
    
    # when the number of n (e.g., site, subj visit) is less than min_n_number_betabinom we do not use betabinomial modeling
    if ((T_Zscore > t_zscore_limit) & all(CVs < 5) & (length(n) >= min_n_number_betabinom)) {
      message("\tBetabinom")
      ExpPer <- rep(ExpPer_Betabinom, N)
      pvalueMethod <- rep("BetaBinom", N)
      pvalue <- pbetabinom
    } else {
      message("\tBinom")
      ExpPer <- rep(ExpPer_Binom, N)
      pvalueMethod <- rep("Binom", N)
      pvalue <- pbinom
    }
  }
  
  Result <- data.frame(rowname, r, n, ObsPer, ExpPer, pvalue, pvalueMethod, stdy_r = sum(r), stdy_n = sum(n))
  rownames(Result) <- NULL
  
  return(Result)
}
