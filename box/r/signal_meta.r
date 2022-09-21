#' SIGNAL_META
#' @export SIGNAL_META
SIGNAL_META <- function(x) {
  list(
    potential_issue = x$potential_issue[[1]],
    cutdt = x$cutdt[[1]],
    country = x$country[[1]],
    paramch = x$paramcd[[1]],
    n_subj = x$n_subj[[1]],
    n_subj_visit = x$n_subj_visit[[1]],
    description = x$description
  )
}
