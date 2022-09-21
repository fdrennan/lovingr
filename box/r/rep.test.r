#' rep.test
#'
#' @param count To Write
#' @param rowsum To Write
#' @param colsum To Write
#' @param total To Write
#'
#' @importFrom stats fisher.test
#'
#' @export rep_test
rep_test <- function(count, rowsum, colsum, total) {

  # perfroms fisher exact for association between a value and a particular site;

  stat <- c(count, rowsum, colsum, total)
  stat1 <- c(count, rowsum - count, colsum - count, total - rowsum - colsum + count)
  Pvalue <- fisher.test(matrix(stat1, 2, 2, byrow = F))$p.value

  oddsRatio <- stat1[1] * stat1[4] / (stat1[2] * stat1[3])
  oddsRatio[oddsRatio == Inf] <- 100

  t <- c(count, colsum, 100 * count / colsum, 100 * count / rowsum, rowsum, total, 100 * rowsum / total, oddsRatio, Pvalue)

  names(t) <- c("Count_Site", "nObs_Site", "PerofSite", "PerofValue", "Count_Study", "nObs_Study", "PerofStudy", "oddsRatio", "Pvalue")
  return(t)
}
