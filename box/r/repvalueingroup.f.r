#' RepValueinGroup.f
#'
#' @param Parname To Write
#' @param x To Write
#' @param group To Write
#' @param padjmethod To Write
#' @param cutoff_padj To Write
#' @param cutoff_count To Write
#' @param cutoff_OddsRatio To Write
#' @param cutoff_PerofSite To Write
#' @param cutoff_PerofValue To Write
#'
#' @importFrom stats p.adjust
#'
#' @export RepValueinGroup.f
RepValueinGroup.f <- function(Parname, data, padjmethod) {
  x <- as.character(data[data$paramcd == Parname, ]$avalc)
  group <- as.character(data[data$paramcd == Parname, ]$siteid)

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
  output <- mapply(rep_test, count = countSeq, rowsum = rowsumSeq, colsum = colsumSeq, total = totalSeq)
  output <- data.frame(t(output))

  # add information
  Values <- rep(as.character(rownames(sitefreq)), times = nGrp)
  Groups <- rep(as.character(colnames(sitefreq)), each = nVal)
  Par <- rep(Parname, length(countSeq))

  # adjusting for multiple comparison;
  output$adjustPval <- p.adjust(output$Pvalue, method = padjmethod)

  # apply flagging rules that based on padj, odds ration and the prevalence of the values;
  # configuration <- filter(configuration, signals == tolower(unique(Par)))

  output$code <- data$code[[1]]


  output <-
    rename(
      output,
      adjusted_p_value = .data$adjustPval,
      site_value_cnt = Count_Site,
      stdy_value_cnt = Count_Study,
      site_value_pct = PerofSite,
      stdy_value_pct = PerofStudy,
      p_value = Pvalue
    ) %>%
    mutate(
      diff_pct = site_value_pct - stdy_value_pct,
    ) %>%
    rename_all(str_to_lower)

  out <- flagger(output, analysis = Parname)
  # out <- map_df(split(output, 1:nrow(output)), ~ flagger(..1, analysis = Parname))


  out <- data.frame(Par, Groups, Values, out)

  return(out)
}
