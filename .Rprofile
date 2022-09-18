source("renv/activate.R")
options(shiny.maxRequestSize = 300 * 1024^2)
options(chatty = FALSE)
options(development = FALSE)
options(base_config = "Config2.xlsx")
options(cache = FALSE)
options(file_regex = "csm[0-9]{6}[a|b|c|x]/datamisc$")
options(bmrn_base_dir='/sassys/cdm/cdmdev/pegpal/pku/165306/csm202201x')
options(datamisc_cache_path = "./datamisc") # use locally

# options(bmrn_base_dir = "/sassys/cdm/cdmdev/bmn111/ach")
# options(bmrn_base_dir='/sassys/cdm/cdmdev/pegpal/pku')
options(base_directory = paste0(
  # getOption("datamisc_cache_path"),
  getOption("bmrn_base_dir")
))
options(cache_path = "./cache/data.rda")
options(analysis_filter = {
  if (getOption("development")) {
    # c("aei", "rgv", "aecnt", "aegap", "vitals", "underdose")
    c("underdose")
    # c("aei", "rgv", "aecnt", "aegap")
    c('aecnt', 'vitals')
    # "vitals"
    # "aei"
    # 'rgv'
    # c("aecnt", "aegap", "aei")
  } else {
    # c("aei")
    # c('aecnt', 'vitals')
    c("aei", "rgv", "aecnt", "aegap", "vitals", "underdose")
  }
})
options(ignoreConfigPath = TRUE)
options(sample_frac = 1)
options(sample_min = 2000000000)

