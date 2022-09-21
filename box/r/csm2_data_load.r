#' csm_data_load
#'
#' @description Used for nearly every analysis,
#' `csm_data_load` is a type-agnostic data loading function.
#'
#' If you need to import a sas7bdat, rda, rds, or csv file into R,
#' then this function can handle it. The reason why I developed a
#' custom process for loading data into R is because when I began development
#' I would convert SAS data to an rda file for quicker loading speeds.
#'
#' However, I don't want to have to update the function when we point to a new
#' type of file. Given that we have variable data paths from Excel, I wanted
#' our program to be able to handle different classes of files.
#'
#' When the argument `normalize` is `TRUE` all columns and converted to snake case
#' and all character columns are converted to lowercase.
#'
#' @examples
#' \dontrun{
#' 
#' csm_data_load(data_path = "data.csv")
#' csm_data_load(data_path = "data.sas7bdat")
#' }
#' 
#' @param data_path A string
#' @family csm_analysis_loop
#' @export csm_data_load
csm_data_load <- function(data_path = NULL, normalize = TRUE) {
  # browser()
  if (Sys.getenv('COPY_OVER')=='TRUE') {
    new_path <- paste0('.', data_path)
    glue('copying to {new_path}')
    dir_create(path_dir(new_path))
    file.copy(data_path, new_path, overwrite = T)
  }
  file_type <- str_to_lower(file_get_extension(data_path))
  file_name <- rev(str_split(data_path, "/")[[1]])[[1]]
  file_original <- str_remove(file_name, paste0(".", file_type))

  # use to use correct import function
  response <- switch(file_type,
    # sas import
    "sas7bdat" = {
      read_sas(data_path)
    },
    # rda import
    "rda" = {
      read_rds(data_path)
    },
    # rds import
    "rds" = {
      read_rds(data_path)
    },
    # csv import
    "csv" = {
      read.csv(data_path)
    },
    # error message
    stop(glue("File Type {file_type} not supported. Only use `sas7bdat`, `rda`, `rds`, `csv` files."))
  )

  if ("site" %in% names(response)) {
    response <- mutate(response, response = as.character(response))
  }

  if ("datacutoff" %in% names(response)) {
    response <- mutate(response,
      datacutoff = as.character(datacutoff),
      cutdt = datacutoff
    )
  }

  if (normalize) {
    response <- mutate_if(response, is.factor, function(x) {
      str_to_lower(as.character(x))
    })
    response <- mutate_if(response, is.character, ~ str_to_lower(str_squish(.)))
    response <- clean_names(response)
  }
  # return as tibble
  # response <- rename_all(response, str_to_lower)
  as_tibble(response)
}
