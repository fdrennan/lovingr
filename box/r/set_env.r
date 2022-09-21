#' set_env
#' @export set_env
set_env <- function(env_name = "t_zscore", default_value = "") {
  env_val <- Sys.getenv(env_name)
  if (env_val != "") {
    message(glue("Using environment variable for {env_name}: {env_val}"))
    response <- Sys.getenv(env_name)
  } else {
    message(glue("Using default variable for {env_name}"))
    response <- default_value
  }
  print(response)
  response
}
