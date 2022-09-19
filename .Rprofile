source("renv/activate.R")
options(shiny.maxRequestSize = 300 * 1024^2)
options(base_config = "Config2.xlsx")
options(file_regex = "csm[0-9]{6}[a|b|c|x]/datamisc$")
options(bmrn_base_dir = "/sassys/cdm/cdmdev/pegpal/pku/165306/csm202201x")

options(
  analysis_dataset_names = c(
    aei = "csmaesttrt.sas7bdat",
    vitals = "csmvs.sas7bdat",
    missdose = "csmmdosest.sas7bdat",
    aegap = "csmaetrt.sas7bdat",
    aecnt = "csm_aecnt.csv",
    diet = "csmdiet.sas7bdat",
    meta = "csmpt.sas7bdat"
  )
)




# options(bmrn_base_dir = "/sassys/cdm/cdmdev/bmn111/ach")
# options(bmrn_base_dir='/sassys/cdm/cdmdev/pegpal/pku')

options(base_directory = paste0(
  getOption("bmrn_base_dir")
))
options(cache_path = "./cache/data.rda")

options(ignoreConfigPath = TRUE)
options(sample_frac = 1)
options(sample_min = 2000000000)


options(dir_chooser_directories = c(`Local Cache` = "./datamisc", `Working Directory` = getwd(), Root = "/"))
