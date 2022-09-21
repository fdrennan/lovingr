#' @export list_to_lower
list_to_lower <- function(list_item) {
  map(
    list_item,
    make_lower
  )
}

#' make_lower
#' @make_lower
make_lower <- function(x) {
  if (is.data.frame(x)) {
    x <- df_lower(x)
  } else {
    x <- map(x, df_lower)
  }
  x
}

#' df_lower
#' @df_lower
df_lower <- function(x) {
  mutate_if(
    x,
    is.character,
    ~ str_trim(str_to_lower(..1))
  )
}
