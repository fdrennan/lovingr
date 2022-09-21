#' csm_zip_package
#' @export csm_zip_package
csm_zip_package <- function(package_path = ".") {
  package_version <- describe_package(package = "csm")[3]
  walk(dir_ls()[sapply(dir_ls(), file_get_extension) == "csv"], file_delete)
  zipr(
    zipfile = glue("../csm-{package_version}.zip"),
    files = file.path(getwd(), dir_ls())
  )
  build_manual(path = getwd())
}
