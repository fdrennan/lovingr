#' @export
TukeyOutliers <- function(data, fence = "outer") {
  box::use(stats)
  lowerq <- stats$quantile(data, na.rm = T)[2]
  upperq <- stats$quantile(data, na.rm = T)[4]
  iqr <- upperq - lowerq # Or use IQR(data)
  # we identify extreme outliers
  if (fence == "outer") {
    threshold.upper <- (iqr * 3) + upperq
    threshold.lower <- lowerq - (iqr * 3)
  } else if (fence == "inner") {
    threshold.upper <- (iqr * 1.5) + upperq
    threshold.lower <- lowerq - (iqr * 1.5)
  } else {
    print("wrong fence name.")
  }


  outlier <- which(data > threshold.upper | data < threshold.lower)
  result <- list(outlier = outlier, fence = c(threshold.lower, threshold.upper), type = fence)
  result
}
