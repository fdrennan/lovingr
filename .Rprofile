source("renv/activate.R")
options(shiny.maxRequestSize = 300 * 1024^2)
options(bmrn_base_dir = "/sassys/cdm/cdmdev/pegpal/pku/165306/csm202201x")
options(csm_version = "0.0.9000")
options(
  analysis_dataset_names = c(
    aei = "csmaesttrt.sas7bdat",
    aei = "csmaest.sas7bdat", # old
    rgm = "csmgmsum.sas7bdat",
    rgv = "csmrgvst.sas7bdat",
    # vitals = "csmvs.sas7bdat",
    # missdose = "csmmdosest.sas7bdat",
    # diet = "csmdiet.sas7bdat",
    # aegap = "csmaetrt.sas7bdat",
    # aegap = "csmae.sas7bdat", # old
    # underdose = "csmexvis.sas7bdat",
    # aecnt = "csm_aecnt.csv",
    # diet = "csmdiet.sas7bdat",
    meta = "csmpt.sas7bdat"
  )
)
options(dir_chooser_directories = c(`Local Cache` = "./datamisc", `Working Directory` = getwd(), Root = "/"))
options(internal_config_path = "Config2.xlsx")
options(file_import_working_directory = c(datamisc = "./datamisc", `Working Directory` = getwd(), Root = "/"))
# aei	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmaest.sas7bdat
# rgm	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmgmsum.sas7bdat
# vitals	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmvs.sas7bdat
# dosereason	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmexvis.sas7bdat
# overdose	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmexvis.sas7bdat
# underdose	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmexvis.sas7bdat
# rgv	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmrgvst.sas7bdat
# aegap	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmae.sas7bdat
# aecnt	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csm_aecnt.csv
# meta	/sassys/cdm/cdmdev/bmn111/ach/111206/csm202108a/datamisc	csmpt.sas7bdat
