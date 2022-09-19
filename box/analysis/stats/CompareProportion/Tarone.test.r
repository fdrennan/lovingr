#' @export Tarone.test
Tarone.test <- function(r, n) {
  # Compute Tarone's Z statistic for overdispersion Beta Binomial;
  # https://rpubs.com/cakapourani/beta-binomial

  p_hat <- sum(r) / sum(n)
  S <- sum((r - n * p_hat)^2 / (p_hat * (1 - p_hat)))
  Z_score <- (S - sum(n)) / sqrt(2 * sum(n * (n - 1)))
  return(Z_score)
}
