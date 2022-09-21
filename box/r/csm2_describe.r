#' describe_package
#' @param package_name A string
#' @export describe_package
describe_package <- function(package_name = "csm") {
  package_metadata <- installed.packages()
  package_metadata <- package_metadata[package_metadata[, 1] == "csm", ]
  package_metadata <- package_metadata[names(package_metadata) %in% c("Package", "LibPath", "Version")]
  walk2(
    names(package_metadata),
    package_metadata,
    ~ {
      .x
      message(glue("{.x} \n\t {.y}\n\n\n"))
    }
  )
}
