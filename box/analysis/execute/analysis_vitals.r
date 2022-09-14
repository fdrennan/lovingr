#' @export
analysis_vitals <- function(input_vs = NULL, configuration = NULL) {
  box::use(dplyr, stringr, purrr, . / analysis_vitals)
  # future$plan(future$multiprocess)
  #
  if ("vsdv" %in% names(input_vs)) {
    split_vs <- input_vs |>
      dplyr$mutate(
        split_on = dplyr$case_when(
          stringr$str_detect(paramcd, "dia") ~ paste0(vsdv, "_", paramcd),
          stringr$str_detect(paramcd, "sys") ~ paste0(vsdv, "_", paramcd),
          stringr$str_detect(paramcd, "bp") ~ paste0(vsdv, "_", paramcd),
          TRUE ~ paramcd
        )
      )
  } else {
    split_vs <- input_vs |>
      dplyr$mutate(
        split_on = paramcd
      )
  }

  split_vs <- split(split_vs, split_vs$split_on)

  out <- purrr$imap_dfr(
    split_vs,
    function(x, y) {
      box::use(. / analysis_vitals, shiny, glue)
      vals <- names(split_vs)
      index <- which(vals == y)
      n_vals <- dplyr$n_distinct(vals)
      shiny$showNotification(
        id = "currentVitals",
        duration = NULL, closeButton = FALSE,
        glue$glue("paramcd: {y} - {index}/{n_vals}")
      )
      out <- analysis_vitals$rep_value_in_group(y, x)
      shiny$removeNotification("currentVitals")
      out
    }
  )

  out
}


#' @export
RepValueinGroup.f <- function(paramcd, data, padjmethod) {
  box::use(dplyr, stringr, purrr, stats)
  box::use(. / analysis_vitals)
  x <- as.character(data[data$paramcd == paramcd, ]$avalc)
  group <- as.character(data[data$paramcd == paramcd, ]$siteid)

  # 1. Perform fisher exact tests of association for each value and site combinations
  # 2. pvalue adjustment
  # 3. flag value & site combination based on flagging rules

  # x is the character values we study;
  # group is the group names, e.g., site id;

  # 0.  Remove missing data first.  assume missing is "NaN";

  ind <- is.na(match(x, "NaN"))
  x <- x[ind]
  group <- group[ind]

  # 1. Perform fisher exact tests of association for each value and site combinations

  # create the frequency counts that constitue the 2x2 contigency tableL
  #                 Group A           Not Group A
  #  Value=y         count           rowsum-count
  #  Value ne y  colsum-count   Totalnumber-rowsum-colsum+count

  sitefreq <- table(x, group, exclude = NULL)

  Valuesum <- apply(sitefreq, 1, sum) # frequency of a value in the study;
  Groupsum <- apply(sitefreq, 2, sum) # number of obs. in a site;
  Totalnumber <- sum(Valuesum) # total number obs in study

  nGrp <- length(unique(group)) # number of sites;
  nVal <- length(unique(x)) # number of unique values

  countSeq <- c(sitefreq) # the vector is build by concatanating columns;
  rowsumSeq <- rep(Valuesum, times = nGrp)
  colsumSeq <- rep(Groupsum, each = nVal)
  totalSeq <- rep(Totalnumber, length(countSeq))

  # use mapply to run fisher exact test of association for all value and group combinations;
  output <- mapply(
    analysis_vitals$rep_test,
    count = countSeq, rowsum = rowsumSeq, colsum = colsumSeq, total = totalSeq
  )
  output <- data.frame(t(output))

  # add information
  Values <- rep(as.character(rownames(sitefreq)), times = nGrp)
  Groups <- rep(as.character(colnames(sitefreq)), each = nVal)
  Par <- rep(paramcd, length(countSeq))

  # adjusting for multiple comparison;
  output$adjustPval <- stats$p.adjust(output$Pvalue, method = padjmethod)

  # apply flagging rules that based on padj, odds ration and the prevalence of the values;
  # configuration <- filter(configuration, signals == tolower(unique(Par)))

  output <-
    dplyr$rename(
      output,
      adjusted_p_value = adjustPval,
      site_value_cnt = Count_Site,
      stdy_value_cnt = Count_Study,
      site_value_pct = PerofSite,
      stdy_value_pct = PerofStudy,
      p_value = Pvalue
    ) |>
    dplyr$mutate(
      diff_pct = site_value_pct - stdy_value_pct
    ) |>
    dplyr$rename_all(stringr$str_to_lower)


  output
}



#' @export
rep_test <- function(count, rowsum, colsum, total) {
  box::use(stats)
  box::use(stringr, . / analysis_vitals)
  # perfroms fisher exact for association between a value and a particular site;
  # box::use(stringr, . / analysis_vitals)
  stat <- c(count, rowsum, colsum, total)
  stat1 <- c(count, rowsum - count, colsum - count, total - rowsum - colsum + count)
  Pvalue <- stats$fisher.test(matrix(stat1, 2, 2, byrow = F))$p.value

  oddsRatio <- stat1[1] * stat1[4] / (stat1[2] * stat1[3])
  oddsRatio[oddsRatio == Inf] <- 100

  t <- c(count, colsum, 100 * count / colsum, 100 * count / rowsum, rowsum, total, 100 * rowsum / total, oddsRatio, Pvalue)

  names(t) <- c("Count_Site", "nObs_Site", "PerofSite", "PerofValue", "Count_Study", "nObs_Study", "PerofStudy", "oddsRatio", "Pvalue")

  t
}



#' @export
rep_value_in_group <- function(paramcd = NULL, input_vs) {
  box::use(stringr, . / analysis_vitals, shiny)
  response <- analysis_vitals$RepValueinGroup.f(paramcd, input_vs, "BY")
  response$paramcd <- paramcd
  response
}
