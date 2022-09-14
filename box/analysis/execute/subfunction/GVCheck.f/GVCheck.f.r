#' @export GVCheck.f
GVCheck.f <- function(GVData, Var_site, Var_by, Var_n, Var_r,
                      name, configuration = NULL) {
  box::use(.. / CompareProportion / CompareProportion)
  box::use(dplyr, stats, purrr)
  T_ZSCORE <- as.numeric(dplyr$filter(configuration, parameter == "t_zscore")$value)
  DataComb <- stats$aggregate(GVData[, c(Var_r, Var_n)], by = list(GVData[, Var_site])[[1]], sum)
  names(DataComb)[1] <- Var_site

  ResComb <- CompareProportion$CompareProportion(
    as.numeric(DataComb[, Var_r]),
    as.numeric(DataComb[, Var_n]),
    DataComb[, Var_site], T_ZSCORE
  )

  GMParameter <- rep("Combined", dim(ResComb)[1])
  ResComb <- data.frame(GMParameter, ResComb)

  result <- ResComb
  # aggreate r and n across subjects for each parameter;
  DatabyPar <- stats$aggregate(
    GVData[, c(Var_r, Var_n)],
    by = purrr$map_dfc(list(GVData[, Var_site], GVData[, Var_by]), ~.),
    sum
  )

  names(DatabyPar)[1:2] <- c(Var_site, Var_by)

  parm <- as.character(unique(DatabyPar[, Var_by]))
  nparm <- length(parm)


  for (i in 1:nparm)
  {
    parname <- parm[i]
    index <- (as.character(DatabyPar[, Var_by]) == parname)
    ResI <- CompareProportion$CompareProportion(
      as.numeric(DatabyPar[index, Var_r]),
      as.numeric(DatabyPar[index, Var_n]),
      DatabyPar[index, Var_site], T_ZSCORE
    )
    GMParameter <- rep(parname, dim(ResI)[1])
    ResI <- data.frame(GMParameter, ResI)
    result <- rbind(result, ResI)
  }

  GVmethod <- rep(name, dim(result)[1])

  result
}
