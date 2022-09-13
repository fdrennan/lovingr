#' @export
run <- function(datapath) {
  box::use(fs, haven, readr, openxlsx)
  datapath_ext <- fs$path_ext(datapath)
  data_for_analysis <- switch(datapath_ext,
    "xlsx" = {
      sheetNames <- openxlsx$getSheetNames(datapath)
      lapply(sheetNames, function(sheetName) {
        list(
          sheetName = sheetName,
          data = openxlsx$read.xlsx(datapath, sheetName)
        )
      })
    },
    "sas7bdat" = list(
      list(
        sheetName = fs$path_file(datapath),
        data = haven$read_sas(datapath)
      )
    ),
    "csv" = {
      list(
        list(
          sheetName = fs$path_file(datapath),
          data = readr$read_csv(datapath)
        )
      )
    },
    list(
      list(
        sheetName = fs$path_file(datapath),
        data = readr$read_file(datapath)
      )
    )
  )
}
