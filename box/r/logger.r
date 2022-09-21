#' delete_when
#'
#' @param file_path A path to a file to monitor
#' @param rows
#'
#' @description
#'
#' A utility function. Given a file path, said file will be
#' deleted when it reaches n rows. This prevents unnecessary disk space
#' from being accumulated.
#'
#' @seealso logger
#'
#' @export delete_when
delete_when <- function(file_path = NULL, rows = 100) {
  if (file_exists(file_path)) {
    is_bigger_than <- nrow(read_csv(file_path)) > rows
    if (is_bigger_than) {
      file_delete(file_path)
    }
  }
}

#' logger
#'
#' @param df A data frame at signal level
#' @param analysis The name of the analysis that the df argument belongs to
#'
#' @description
#'
#' If a log directory does not exist, it creates it. The default log name
#' is logs/logs-analysisname.csv If the REQUIRE_PROMPT environment variable
#' is TRUE, then the user will be prompted to make a decision about whether
#' or not the prompt should be used in each interation of the flagging process.
#'
#' The suspension of the program can be useful, to understand the data more clearly,
#' check a column name, etc. This process is off by default.
#'
#' @export logger
logger <- function(df, analysis) {
  if (!dir_exists("logs")) {
    dir_create("logs")
  }

  file_path <- glue("logs/logs-{analysis}.csv")

  df$time <- Sys.time()

  delete_when(
    file_path = file_path,
    rows = 100
  )

  if (file_exists(file_path)) {
    write_csv(df, file = file_path, append = TRUE)
  } else {
    write_csv(df, file = file_path)
  }
}

#' flagger
#'
#' @description The flagger function applies flags to vital signs, dosing,
#' aei, aecount, and aegap.
#'
#' flagger takes y, a dataframe of one signal type (saei, togr1, sysbp, etc.),
#' with flagging code from the excel sheet in the code column.
#'
#' There are a few things wrong with this function. attach is generally considered
#' bad practice and it is very slow computationally. Preferably, we move to using
#' with(df, cols) == TRUE then FLAG
#'
#' @param y A data frame with one signal/paramcd type, with code from excel sheet
#' @param analysis The name of the analysis, aei, aecount, underdose, etc.
#'
#' @examples
#' \dontrun{
#' 
#' # from analysis_aecount
#' split_data <- split(count_long, 1:nrow(count_long))
#' flags <- map_df(split_data, flagger, analysis = "aecnt")
#' flags
#' }
#' 
#' @seealso logger
#' @export flagger
flagger <- function(y, analysis) {

  if (analysis == "aecnt") {
    y <- rename(y, adjusted_p_value = adjp)
  }

  if (analysis == "aegap") {
    y <- rename(y, site_avg = grp_mean, stdy_avg = grandmean, diff_avg = diff)
  }

  if (analysis == "vitals") {}

  code <- unlist(strsplit(y$code, ";"))
  regex_get_flag <- "\\)\\{.*\\}$"
  flags <- str_remove_all(code, " ")
  flags <- str_extract(flags, regex_get_flag)
  flags <- as.numeric(str_remove_all(flags, "[\\), =,flag,\\}, \\{, \\}]"))
  clean_code <- str_remove(str_remove(str_remove_all(code, " "), "^if\\("), regex_get_flag)
  mapping <- tibble(code = clean_code, flags = flags) %>%
    distinct()

  mapping <- mapping %>%
    mutate(
      code = str_replace_all(code, "<", " < "),
      code = str_replace_all(code, "<", " < "),
      code = str_replace_all(code, ">", " > "),
      code = str_replace_all(code, "&", " & "),
      code = str_replace_all(code, "> =", " >= "),
      code = str_replace_all(code, "< =", " <= "),
      code = str_replace_all(code, "\\|", " \\| ")
    )

  y$flag <- split(mapping, 1:nrow(mapping)) %>%
    keep(~ nrow(.) > 0) %>%
    map(function(x) {
      if_else(
        eval(parse(text = paste0("with(y, ", x$code, ")"))),
        x$flags, 0
      )
    }) %>%
    as_tibble() %>%
    rowSums()

  y
}
