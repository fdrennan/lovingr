copy_config <- function(xlsxFile = "Config3.xlsx") {
  file.copy(
    openxlsx::read.xlsx(xlsxFile = xlsxFile, 4)$final,
    {
      file_to_make <- paste0("./datamisc", openxlsx::read.xlsx(xlsxFile = xlsxFile, 4)$final)
      fs::dir_create(fs::path_dir(file_to_make), recurse = T)
      file_to_make
    }
  )
}
