#' @export analysis_underdose
analysis_underdose <- function(dose_subj = NULL,
                               configuration = NULL) {
  box::use(dplyr)
  box::use(. / subfunction / TukeyOutliers / TukeyOutliers)
  cutdt <- unique(dose_subj$cutdt)
  country_mapping <- dplyr$distinct(dplyr$select(dose_subj, studyid, country, site = siteid))

  dose_subj$siteid <- as.factor(dose_subj$siteid)
  dose_subj$subject <- as.factor(dose_subj$subject)

  names(dose_subj)

  fence <- dplyr$filter(configuration$parameters, parameter == "fence")$value
  cutoff_perplanned <- as.numeric(dplyr$filter(configuration$parameters, parameter == "cutoff_perplanned")$value)
  tz_score <- as.numeric(dplyr$filter(configuration$parameters, parameter == "t_zscore")$value)
  DataCutDate <- as.Date(dose_subj$cutdt[1], origin = "1960-1-1")

  dose <- dose_subj[dose_subj$exdose >= 0 & !is.na(dose_subj$exdose), ]
  diff <- dose$exdose - dose$expdose
  perdiff <- 100 * diff / dose$expdose
  dose <- data.frame(dose, diff, perdiff)
  n <- table(dose$siteid)

  Outliers_info <- TukeyOutliers$TukeyOutliers(dose$extdose, fence = fence)
  Outliers <- Outliers_info$outlier
  outliers_low <- Outliers[diff[Outliers] < 0 & dose[Outliers, ]$extdose < cutoff_perplanned]
  # OutliersHigh=Outliers[diff[Outliers]>0 & dose[Outliers,]$extdose > 100+ (100 - c1)]


  OUTLIERS_LOW <- unique(dose[outliers_low, c("studyid", "siteid", "subject")]) # outliers_low.csv

  # write.csv(unique(dose[OutliersHigh, c("STUDYID", "siteid", "subject")]),
  #         paste(outpath, "/OutliersHigh.csv", sep="" ), row.names = F, quote = F)

  # print(paste( "number of overdose outliers=", length(OutliersHigh), sep=""))
  print(paste("number of underdose outliers=", length(outliers_low), sep = ""))

  r <- table(dose[outliers_low, "siteid"])
  # n <- n[names(n) %in% names(r)]
  site_result <- CompareProportion(as.numeric(r), as.numeric(n), names(n), tz_score)

  site_result$fence <- fence
  site_result$cutoff_perplanned <- cutoff_perplanned

  # siteinfo <- unique(dose[, c("COUNTRY", "siteid", "STUDYID", "CUTDT")])
  # siteinfo$CUTDT <- as.Date(siteinfo$CUTDT, origin = "1960-1-1")

  SITE_RESULT <- site_result # site_result.csv

  n <- table(dose$subject)
  r <- table(dose$subject[diff < 0])
  # n <- n[names(n) %in% names(r)]
  jk1 <- CompareProportion(as.numeric(r), as.numeric(n), names(n), tz_score)



  jk2 <- aggregate(dose$perdiff, by = list(dose$subject), mean)
  jk <- merge(jk1, jk2, by.x = "rowname", by.y = "Group.1")
  dimnames(jk)[[2]] <- c(
    "Subject", "Number of UnderDose", "Number of Dose", "Percentage", "Expected %",
    "P value", "Method", "stdy_r", "stdy_n", "Average % underdose"
  )

  jk$site <- sapply(strsplit(as.character(jk$Subject), "-", fixed = T), "[", 1)

  SUBJECT_UNDERDOSE <- jk #  "/subjectunderdose.csv"


  SITE_RESULT <-
    SITE_RESULT |>
    mutate(
      site_pct = ObsPer,
      stdy_pct = ExpPer,
      diff_pct = site_pct - stdy_pct
    )

  SITE_RESULT$p_value <- SITE_RESULT$pvalue
  SITE_RESULT$code <- configuration$configuration$code
  SITE_RESULT <- split(SITE_RESULT, 1:nrow(SITE_RESULT))
  SITE_RESULT <- map_df(SITE_RESULT, flagger, analysis = "underdose")

  message("Additional summaries in underdose currently not exported,
          but generated displayed for 2 seconds below.")
  Sys.sleep(1)
  additional_summaries <- list(
    SUBJECT_UNDERDOSE = SUBJECT_UNDERDOSE,
    OUTLIERS_LOW = OUTLIERS_LOW
  )

  iwalk(additional_summaries, ~ {
    cat(..2)
    cat("\n\n")
    glimpse(..1)
    cat("\n")
  })
  Sys.sleep(2)

  SITE_RESULT$cutdt <- cutdt
  SITE_RESULT <- rename(SITE_RESULT, site = rowname)
  SITE_RESULT$paramcd <- "underdose"
  SITE_RESULT <- inner_join(
    SITE_RESULT,
    mutate(country_mapping, site = as.character(site))
  )
  SITE_RESULT
}
