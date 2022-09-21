#' insufficient_load
#'
#' @description This function simulates the loading of the csm
#' package without actually building the package. This is generally
#' useful only when running R on limited systems - like the BioMarin
#' Corporate Desktop.
#'
#' @param r_dir The R Directory, this is only to be run from the working
#' directory of the package. Defaults to "./R
#'
#' @export insufficient_load
insufficient_load <- function(r_dir = "R") {
  pkgs <-
    c(
      "foreign",
      "MASS",
      "VGAM",
      "sas7bdat",
      "emmeans",
      "readr",
      "stringr",
      "purrr",
      "openxlsx",
      "rlang",
      "tibble",
      "xml2",
      "glue",
      "openxlsx",
      "dplyr",
      "jsonlite",
      "knitr",
      "tidyr",
      "multcomp",
      "fs",
      "anytime"
    )
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
  walk(fs::dir_ls(r_dir), function(x) {
    message(glue("Importing: {x}"))
    source(x)
  })
}
