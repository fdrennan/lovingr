source("renv/activate.R")
options(shiny.maxRequestSize = 300 * 1024^2)
options(chatty = FALSE)
options(development = TRUE)
options(base_config = "Config.xlsx")
options(cache = getOption("development"))
options(file_regex = "csm[0-9]{6}[a|b|c]/datamisc$")
options(datamisc_cache_path = "./datamisc")
options(bmrn_base_dir = "/sassys/cdm/cdmdev/bmn111/ach")
options(base_directory = paste0(
  getOption("datamisc_cache_path"),
  getOption("bmrn_base_dir")
))
options(cache_path = "./cache/data.rda")
options(analysis_filter = {
  if (getOption("development")) {
    # Cc("aei", "rgv", "vitals", "rgm", "underdose", "aegap", "aecnt")
    # c("aei", "rgv", "aecnt", "aegap")
    "aegap"
  } else {
    c("aei")
    # c("aei", "rgv", "vitals", "rgm", "underdose", "aegap", "aecnt")
  }
})
options(ignoreConfigPath = TRUE)
