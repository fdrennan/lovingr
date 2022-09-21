
box::use(.. / box / R / study_signal_generator)
study_signal_generator(
  path_csm_excel = "../r-template/configs/Config3.xlsx",
  repository = "../r-template/csm-output-repo"
)

tolower(base::list.files("box/R"))

purrr::map2(
  base::list.files("box/R", full.names = T),
  tolower(base::list.files("box/R", full.names = T)),
  function(x, y) {
    if (!dir.exists(fs::path_dir(y))) {
      fs::dir_create(y, recurse = T)
    }
    file.copy(x, y)
  }
)

warnings()
