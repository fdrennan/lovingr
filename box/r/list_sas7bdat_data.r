#' list_sas7bdat_data
#' @export list_sas7bdat_data
list_sas7bdat_data <- function(base_dir = "csm_dev/datacollections/") {
  file_paths <- as.character(
    dir_ls(
      base_dir,
      recurse = TRUE,
      type = "file",
      regex = "sas7bdat$"
    )
  )

  filenames <- tibble(
    file_paths = file_paths,
    directory = paste0(path_dir(file_paths), "/"),
    extension = file_get_extension(file_paths),
    name = str_remove_all(str_remove_all(file_paths, directory), paste0(".", extension))
  )

  filenames <-
    filenames %>%
    select(name, file_paths, directory, extension) %>%
    arrange(name, file_paths)

  filenames
}
