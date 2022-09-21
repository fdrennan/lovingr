#' analysis_gm
#' @importFrom dplyr as_tibble
#' @param file_name Path to repeat growth data
#' @family csm_analysis_loop
#' @export analysis_gm
analysis_gm <- function(analysis_data = NULL, configuration = NULL, NREP = 3) {
  csm_cli_header("RUNNING BUILD REPEAT GROWTH ANALYSIS")
  analysis_data <- mutate(analysis_data,
    signals = as.character(paramcd),
    gmtest = signals, sitenumber = siteid
  )
  analysis_data <- mutate(
    analysis_data,
    signals = case_when(
      signals == "1" ~ "NineTestCombined",
      TRUE ~ signals
    )
  )
  is_default <- str_detect(configuration$configuration$signals, "select")
  if (is_default) {
    analysis_data <- bind_cols(analysis_data, select(configuration$configuration, -signals))
  } else {
    analysis_data <- inner_join(analysis_data, configuration$configuration)
  }

  # add to configuration
  data <- analysis_data[analysis_data$nrep == NREP, ]
  if (nrow(data) == 0) {
    nrep <- max(unique((analysis_data$nrep)))
    message(glue("In analysis_vitals NREP={NREP} but max(analysis_data$nrep)={nrep} using {nrep}"))
    data <- analysis_data[analysis_data$nrep == nrep, ]
  }
  data <- as_tibble(data)
  # glimpse(data)

  # perform the analysis within each test;
  data$gmtest <- gsub("[\r\n]", "", data$gmtest) # remove "new line" from testname

  gmtest <- unique(data$gmtest)
  gmtest <- gmtest[!gmtest == ""]
  nGMTEST <- length(gmtest)

  r1 <- (data$stdest == 0) + 0
  r <- tapply(r1, data$sitenumber, sum)
  n <- tapply(r1, data$sitenumber, length)
  jj <- CompareProportion(r, n, rownames(r), as.numeric(filter(configuration$parameters, parameter == "t_zscore")$value))

  testname <- rep("NineTestCombined", length(n))
  result <- data.frame(testname, jj)
  for (i in 1:nGMTEST) {
    # print(i)
    subdata <- data[data$gmtest == gmtest[i], ]
    r1 <- (subdata$stdest == 0) + 0
    r <- tapply(r1, subdata$sitenumber, sum)
    n <- tapply(r1, subdata$sitenumber, length)
    r <- r[!is.na(r)]
    n <- n[!is.na(n)]
    rowname <- rownames(r)
    jj1 <- CompareProportion(as.numeric(r), as.numeric(n), rowname, as.numeric(filter(configuration$parameters, parameter == "t_zscore")$value))
    testname <- rep(as.character(gmtest[i]), sum(names(n) %in% jj1$rowname))
    jj2 <- data.frame(testname, jj1)
    result <- rbind(result, jj2)
  }

  # flagging;
  # 1. pvalue <0.05;
  # different in percentage >=15%;
  # at least repeated three times;
  # at least have 10 subject visits;
  # Let y=abs(perdiff) the absolute percentage difference between site and study for each growth measurement,
  # rule is: Max(y) >35 or Sum (the top two y) > 55  or sum (top three y) > 75



  result <- mutate(result, signals = as.character(testname))
  result <- mutate(
    result,
    signals = case_when(
      signals == "1" ~ "NineTestCombined",
      TRUE ~ signals
    )
  )

  if (is_default) {
    result <- bind_cols(result, select(configuration$configuration, -signals))
  } else {
    result <- inner_join(result, configuration$configuration)
  }



  result <-
    result %>%
    mutate(
      site_pct = ObsPer,
      stdy_pct = ExpPer,
      diff_pct = site_pct - stdy_pct,
      p_value = pvalue,
      site_max_diff_pct = 10000,
      sum_site_top2_diff_pct = 10000,
      sum_site_top3_diff_pct = 10000
    )

  result <- flagger(result, analysis = "rgm")

  if (sum(result$flag) == 0) {
    stop("No flags created, try a different configuration.")
  }

  # FIRST FLAGGING

  result$perdiff <- result$ObsPer - result$ExpPer


  #### SECOND FLAGGING
  result1 <- result %>%
    # filter(flag == 1) %>%
    mutate(selectedPerdiff = perdiff)
  result2 <- result1[order(result1$rowname, result1$selectedPerdiff, decreasing = T), ]


  # x is sorted from large to small
  sumNvalues <- function(x, n) {
    sum(x[1:n])
  }

  top1 <- aggregate(result2$selectedPerdiff, list(result2[, c("rowname")]), FUN = max)
  names(top1) <- c("rowname", "max")
  top2 <- aggregate(result2$selectedPerdiff, list(result2[, c("rowname")]), FUN = sumNvalues, n = 2)
  names(top2) <- c("rowname", "sum_max2")
  top3 <- aggregate(result2$selectedPerdiff, list(result2[, c("rowname")]), FUN = sumNvalues, n = 3)
  names(top3) <- c("rowname", "sum_max3")
  tops <- merge(merge(top1, top2, by = "rowname"), top3, by = "rowname")

  tops <-
    tops %>%
    mutate_if(is.numeric, ~ if_else(is.na(.), 0, .))



  # tops$flag_site <- tops$max > 35 | tops$sum_max2 > 55 | tops$sum_max3 > 75

  result3 <- merge(result1, tops, by = "rowname")

  csm_cli_header("REPEAT GROWTH ANALYSIS COMPLETE")
  as_tibble(result3)

  out <-
    result3 %>%
    mutate(
      site_max_diff_pct = max,
      sum_site_top2_diff_pct = sum_max2,
      sum_site_top3_diff_pct = sum_max3
    )


  out <- flagger(out, analysis = "rgm")

  out$cutdt <- analysis_data$cutdt[[1]]

  out <-
    out %>%
    rename(site = rowname)
}

if (FALSE) {
  # library(csm)

  home_path <- Sys.getenv("CSM_HOME")

  csm_data_repeat_growth_testing <- analysis_gm(file_name = file.path("csm_dev", "cache", "summary.sas7bdat.rda"))

  gm_output <- transmute(
    .data = csm_data_repeat_growth_testing,
    site = rowname,
    paramcd = testname,
    summary_level = "subj_visit",
    r,
    n_subj = 0,
    n_subj_visit = n,
    obs_pct = ObsPer,
    stdy_pct = ExpPer,
    diff_pct = obs_pct - stdy_pct,
    p_value = pvalue,
    method = pvalueMethod,
    flag = 0
  )
}
