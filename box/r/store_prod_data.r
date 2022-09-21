#' store_prod_data
#' @export store_prod_data
store_prod_data <- function(data_path, folder) {
  mastersheet <- csm_mastersheet_generate(data_path)
  dir_create(folder)
  file_copy(data_path, paste0(folder, "/", path_file(data_path)), overwrite = TRUE)
  file_copy(
    mastersheet$data_paths$final,
    paste0(folder, "/", path_file(mastersheet$data_paths$final)),
    overwrite = TRUE
  )
  list(mastersheet)
}


if (FALSE) {
  dir_data <-
    dir_info("/rsys/", regexp = "_CSMConfig.xlsx", recurse = T, fail = F) %>%
    transmute(path, file_name = path_file(path), dir_path = str_extract(
      file_name, "^.+_"
    ))
  resp <-
    dir_data %>%
    rowwise() %>%
    mutate(
      scoreboard = tryCatch(expr = {
        store_prod_data(path, glue("proddata/{dir_path}"))
      }, error = function(err) {
        list(as.character(err))
      })
    )
}
