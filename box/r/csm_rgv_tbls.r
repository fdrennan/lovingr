#' get_scoreboard
#' @export
get_scoreboard <- function(path,fn){
  scbd <- read_xlsx(paste(path,fn,sep="/"), sheet="Scoreboard") %>%
    filter(potential_issue == "identical gv values")
  if (nrow(scbd)==0) return(NULL) else {
    names(scbd)[8] <- "summary_stats"
    return(scbd[,c(2,3,4,7,8)])
  }
}

#' apply_splits
#' @export 
apply_splits <- function(x, y) {
  map2(x, y, function(x, y) {
    if (any(map_lgl(list(x, y), is.null))) {
      return(NA)
    }
    
    all_data <- bind_rows(x, y)
    if (all(is.na(all_data$chgc))) {
      return(tibble())
    }
    
    all_data <- data.table(all_data)
    
    ds_has_repeat_chg <-
      all_data[, .N, by=list(paramcd, siteid, subject, chgc)][N>1]
    
    if (nrow(ds_has_repeat_chg) > 0) {
      
      ds_has_repeat_chg <-
        all_data[, .N, by=list(paramcd, siteid, chgc)]
      
      ds_has_repeat_chg$first_visit <- min(all_data$visitnum)
      ds_has_repeat_chg$last_visit <- max(all_data$visitnum)
    }
    toc()
    
    
    
    return(ds_has_repeat_chg)
    
  }
  
  )
  
}

#' patt1
#' @export patt1
patt1 <- function(ds){ # ds <- site_sig_w
  return(table(unlist(apply(ds,2,table)))) # nreps of same value per visit
  # rm(ds)
}

#' patt2
#' @export patt2
patt2 <- function(ds){
  return(table(unlist(apply(ds,1,table)))) # nreps of same value per subject
}

#' patt3
#' @export patt3
patt3 <- function(ds){
  return(table(table(ds))) # nreps of same value across study
}

#' patt4
#' @export
patt4 <- function(ds){
  # ds <- patt4_dat
  tmp <- vector("list",1)
  tmp[[1]] <- split((ds_lc <- rename_all(ds, tolower)), ds_lc$visitnum)
  out <- map(tmp, ~ apply_splits(., lead(.)))
  out <- map_dfr(out, ~ bind_rows(discard(., ~ all(is.na(.)))))
  return(out)
  # rm(ds)
}

#' patt4.colmns
#' @export
patt4.colnms <- function(n_sub){
  col1nm <- "n1r2"
  if (n_sub==1) {
    return(col1nm) 
  } else {
    colnms <- col1nm
    for (ii in 2:n_sub){
      nm <- paste0("n",ii,"r2")
      colnms <- c(colnms, nm) # c("n1r2", "n2r3", "n3r2", ..., "n#r2") where # = n_sub
    }
    return(colnms)
  }
}

#' patt_res
#' @export 
patt_res <- function(pattnum, act, sim){
  # pattnum <- 2; act <- patt2.act; sim <- sim_res[[2]]
  if (all(act==0)) {
    res1 <- data.frame(Result=paste0("Pattern ",pattnum," did not occur in this data"))
  } else {
    dist <- sim
    # ndist <- apply(dist, 2, length)
    # nsim <- ndist[1]
    # dist.sort <- apply(dist, 2, sort)
    ecd <- apply(dist, 2, ecdf)
    pval <- 1 - mapply(function(x, y) y(x), as.list(act), ecd)
    mn <- apply(dist, 2, mean, na.rm=TRUE)
    # sd <- apply(dist, 2, sd, na.rm=TRUE)
    # var <- apply(dist, 2, var, na.rm=TRUE)
    # min <- apply(dist, 2, min, na.rm=TRUE)
    # q.001 <- dist.sort[floor(0.001*nsim),]
    # q.01 <- dist.sort[floor(0.01*nsim),]
    # q.025 <- dist.sort[floor(0.025*nsim),]
    # q.05 <- dist.sort[floor(0.05*nsim),]
    # q.1 <- dist.sort[floor(0.1*nsim),]
    # q.25 <- dist.sort[floor(0.25*nsim),]
    # med <- apply(dist, 2, median, na.rm=TRUE)
    # q.75 <- dist.sort[ceiling(0.75*nsim),]
    # q.9 <- dist.sort[ceiling(0.9*nsim),]
    # q.95 <- dist.sort[ceiling(0.95*nsim),]
    # q.975 <- dist.sort[ceiling(0.975*nsim),]
    # q.99 <- dist.sort[ceiling(0.99*nsim),]
    # q.999 <- dist.sort[ceiling(0.999*nsim),]
    # max <- apply(dist, 2, max, na.rm=TRUE)
    # res <- data.frame(rbind(act, ndist, pval, mn, sd, var, min, q.001, q.01, q.025, q.05, q.1, q.25, 
    #                         med, q.75, q.9, q.95, q.975, q.99, q.999, max))
    pvalLE05 <- which((act != 0) && (pval <= 0.05))
    if (length(pvalLE05)==0){
      res1 <- data.frame(Result=paste0("No extreme occurrences of Pattern ",pattnum," in this data"))
    } else {
      res <- data.frame(rbind(act, pval, mn))
      lbl <- if(pattnum %in% c(1,4)) "subj" else "visit"
      act.nms <- colnames(act)
      colnms.res <- paste(act.nms,ifelse(act.nms=="1",lbl,paste0(lbl,"s")))
      # row.names(res) <- c("actual", "N BS samples", "act.pval", "BS.mn", "BS.sd", "BS.var", "BS.min", "0.1%", "1%", 
      #                     "2.5%", "5%", "10%", "25%", "BS.med", "75%", "90%", "95%", "97.5%", "99%", "99.9%", "BS.max")
      row.nms <- c("Freq", "p-value", "dist mean")
      res1 <- cbind(row.nms,res[,pvalLE05]) # everything character
      colnames(res1) <- c("Statistic",colnms.res[pvalLE05])
    }
  }
  return(as.data.frame(res1))
  # rm(act,sim,dist,ndist,nsim,dist.sort,ecd,pval,mn,sd,var,min,q.001,q.01,q.025,q.05,q.1,q.25,med,q.75,q.9,q.95,q.975,q.99,q.999,max,res,res1)
}

#' sim_data
#' @export
sim_dat <- function(n_sim, study_dat_t){ # what is called simulation here is actually bootstrapping
  # n_sim <- 1000; study_dat_t <- site_sig
  # get simulation parameter values for this study_dat_t dataset
  nsub <- length(unique(study_dat_t$SUBJECT))
  nvis <- length(unique(study_dat_t$VISITNUM))
  nobs <- length(study_dat_t$CHGC) # number of non-NA values (non-repeated and repeated) in array
  nval <- length(unique(study_dat_t$CHGC))
  # following reproduces structure of actual data array
  study_dat_w <- as.data.frame(pivot_wider(study_dat_t, id_cols=SUBJECT, names_from=VIS, values_from=CHGC))[,-1]
  study_dat_w <- study_dat_w[,sort(names(study_dat_w))]
  # where are NAs in actual data array
  has_na <- any(is.na(study_dat_w))
  if (has_na) {
    loc_na <- which(is.na(study_dat_w))
    simvec_ord <- c((1:(nsub*nvis))[-c(loc_na)], loc_na)
  } else simvec_ord <- 1:(nsub*nvis)
  # holders for sim results
  sim_arrays <- matrix(data=rep(0,(nvis+2)*nsub*n_sim),nrow=nsub*n_sim,ncol=nvis+2,
                       dimnames=list(NULL,c("SIM_NUM","SUBJ_NUM",names(study_dat_w))))
  # patt1 = occurrence of same value per visit across subjs
  # the result per sim will be the freq of nbr of subjs per visit with the same value. From each column's
  # bootstrapped dist, the pctl of the actual result will be determined using ecdf, and summary stats for
  # the bootstrapped dist will also be estimated and reported
  patt1.sim <- matrix(data=rep(0,nsub*n_sim),nrow=n_sim,ncol=nsub,dimnames=list(NULL,c(1:nsub)))
  # patt2 = occurrence of same value per subj across visits. Explanation similar to patt1
  patt2.sim <- matrix(data=rep(0,nvis*n_sim),nrow=n_sim,ncol=nvis,dimnames=list(NULL,c(1:nvis)))
  # patt3 = occurrence of same value across study. Theoretically, 1 value could be repeated nobs times.  Explanation similar to patt1
  patt3.sim <- matrix(data=rep(0,nobs*n_sim),nrow=n_sim,ncol=nobs,dimnames=list(NULL,c(1:nobs)))
  # patt4 = occurrence of runs of length 2 of same value in 1:nsub. Explanation similar to patt1
  # for patt4.sim, ncol=nsub because theoretically all subjects could have the same run of length 2
  patt4.sim <- matrix(data=rep(0,nsub*n_sim),nrow=n_sim,ncol=nsub,dimnames=list(NULL,patt4.colnms(nsub)))
  
  set.seed(9272021)
  for (nn in 1:n_sim){ # nn <- 0;   nn <- nn+1
    sim_vec <- sample(1:nval, nobs, replace=TRUE)
    simvec <- if (has_na) c(sim_vec, rep(NA, nsub*nvis - nobs))[order(simvec_ord)] else sim_vec
    sim_mat <- matrix(simvec, nrow=nsub, byrow=FALSE, dimnames=list(NULL, VIS=names(study_dat_w)))
    sim_array <- cbind(SIM_NUM=nn, SUB_NUM=1:nsub, sim_mat)
    sim_arrays[((nn-1)*nsub+1:nsub),] <- sim_array
    tbl1 <- patt1(sim_mat)
    patt1.sim[nn, c(as.integer(names(tbl1)))] <- tbl1
    tbl2 <- patt2(sim_mat)
    patt2.sim[nn, c(as.integer(names(tbl2)))] <- tbl2
    tbl3 <- patt3(sim_vec)
    patt3.sim[nn, c(as.integer(names(tbl3)))] <- tbl3
    # replace actual CHGC values with bootstrap values
    patt4_dat <- study_dat_t[,-which(names(study_dat_t)=="CHGC")]
    patt4_dat$CHGC <- as.character(sim_vec)
    patt4resdat <- patt4(patt4_dat)
    tbl4 <- if (nrow(patt4resdat) > 0) table(patt4resdat$n_subj) else matrix(0,ncol=1,dimnames=list(NULL,"1"))
    patt4.sim[nn, c(as.integer(names(tbl4)))] <- tbl4
  }
  return(list(patt1.sim, patt2.sim, patt3.sim, patt4.sim, sim_arrays))
  # rm(n_sim,study_dat_t,study_dat_w,nsub,nvis,nobs,nval,has_na,loc_na,simvec_ord,sim_arrays,patt1.sim,patt2.sim)
  # rm(patt3.sim,patt4.sim,nn,sim_vec,simvec,sim_mat,sim_array,tbl1,tbl2,tbl3,patt4_dat,patt4resdat,tbl4)
}

#' csm_rgv_tbls
#' @export 
csm_rgv_tbls <- function(proj, ind, study, cutdt){
  # proj <- "bmn111"; ind <- "ach"; study <- "111205"; cutdt <- "202110"
  fldr <- paste0("csm",cutdt,"a")
  cdm_path <- paste("/sassys/cdm/cdmdev",proj,ind,study,fldr,sep="/")
  rsys_path <- paste("/rsys/qsci/csm",proj,study,fldr,sep="/")
  scbd_path <- paste(rsys_path,"output/SCOREBOARD",sep="/")
  scbd_files <- dir(scbd_path, pattern=".xlsx")
  scbd_file <- scbd_files[which(substr(scbd_files,1,4)==substr(cutdt,1,4))]
  wb_name_one <- paste0(study, "_", cutdt, "_actual and bs data.xlsx")
  wb_name_two <- paste0(study, "_", cutdt, "_flagged RGV signals.xlsx")
  
  
  path_one = paste0(rsys_path, "/output/RGV/", wb_name_one)
  path_two = paste0(rsys_path, "/output/RGV/", wb_name_two)
  
  cli_alert_info('path_one: {path_one}')
  cli_alert_info('path_two: {path_two}')
  
  if (!length(scbd_file)) {
    stop('scoreboard does not exist in path')
  }
  # browser()
  cli_alert_info('Grabbing scoreboard at {scbd_path}')
  gv_flagged <- get_scoreboard(file.path(scbd_path, scbd_file))
  if (is.null(gv_flagged)) {
    stop(paste0("Scoreboard file ",paste(scbd_path,scbd_file,sep="/"), " does not contain any flagged RGV sites."))
  } 
  
  # get data for flagged sites/signals
  gv_sites <- gv_flagged$site
  cli_alert_info('grabbing sas data')
  gv_data <- read_sas(paste(cdm_path,"datamisc/csmrgv.sas7bdat",sep="/")) %>% 
    filter(grepl('WEEK', toupper(VISIT)) & VISITNUM>1 & !is.na(CHG)) %>% 
    select(SUBJECT,SITEID,VISITNUM,PARAMCD,CHG,CHGC) # View(gv_data)
  
  # save actual and bootstrap data tables into one workbook. For each flagged site/signal combination, there will be
  # 2 worksheets, the first containing the actual table of rgv results and the second the bootstrapped data tables.
  # Timewise, if another pattern is added, or maybe to reformat existing output, it will be easier/faster to use the 
  # saved data, rather than regenerate it
  wb.dat <- createWorkbook()
  wbdat_wsn <- 0
  
  # put all output into one workbook. For each flagged site/signal combination, there will be
  # one worksheet, containing the actual table of rgv results and 4 summary tables, one for each pattern
  wb.res <- createWorkbook()
  ws_n <- 0
  for (ii in 1:length(gv_sites)){ # ii <- 0;   ii <- ii+1
    signals_ii <- gsub("\n",", ",gv_flagged$signals[ii])
    sigs_ii <- toupper(unlist(str_split(paste(signals_ii,collapse=", "), ", ")))
    n_sigs <- length(sigs_ii)
    cli_alert_info('ii={ii}')
    for (jj in 1:n_sigs){ # jj <- 0;     jj <- jj+1
      cli_alert_info('jj={jj}')
      # get and process data for flagged signal(s) at site
      site_sig <- filter(gv_data, SITEID==gv_sites[ii], PARAMCD==sigs_ii[jj]) %>%
        mutate(SUBJ=substr(SUBJECT,6,9), 
               VIS=paste0("W",ifelse(nchar(VISITNUM)==2,"0",""),VISITNUM),
               CHGC_=substr(CHGC,3,8))
      
      if(nrow(site_sig)==0) {
        break
      }
      
      # look for patterns in simulated data with given structure
      sim_res <- sim_dat(1000, site_sig)
      
      # actual data as nsub*nvis array with NAs for missing visits
      site_sig_w <- as.data.frame(pivot_wider(site_sig, id_cols=SUBJECT, names_from=VIS, values_from=CHGC_))
      site_sig_w <- site_sig_w[,sort(names(site_sig_w))]
      
      # save actual and bs data to wb.dat
      wbdat_wsn <- wbdat_wsn + 1
      addWorksheet(wb.dat, sheetName=paste(gv_sites[ii], sigs_ii[jj], "act", sep="_"))
      writeDataTable(wb.dat, sheet=wbdat_wsn, x=site_sig_w, colNames=TRUE, withFilter=FALSE) # actual data
      wbdat_wsn <- wbdat_wsn + 1
      addWorksheet(wb.dat, sheetName=paste(gv_sites[ii], sigs_ii[jj], "bs", sep="_"))
      writeDataTable(wb.dat, sheet=wbdat_wsn, x=as.data.frame(sim_res[[5]]), colNames=TRUE, withFilter=FALSE) # bs data
      
      # patterns in actual data
      cli_alert_info('length unique')
      nsub <- length(unique(site_sig$SUBJECT))
      nvis <- length(unique(site_sig$VISITNUM))
      nobs <- length(site_sig$CHGC)
      # nval <- length(unique(site_sig$CHGC))
      cli_alert_info('patt*.act')
      patt1.act <- matrix(data=rep(0,nsub),nrow=1,ncol=nsub,dimnames=list(NULL,c(1:nsub)))
      patt2.act <- matrix(data=rep(0,nvis),nrow=1,ncol=nvis,dimnames=list(NULL,c(1:nvis)))
      patt3.act <- matrix(data=rep(0,nobs),nrow=1,ncol=nobs,dimnames=list(NULL,c(1:nobs)))
      # for patt4.act, ncol=nsub because theoretically all subjects could have the same run of length 2
      patt4.act <- matrix(data=rep(0,nsub),nrow=1,ncol=nsub,dimnames=list(NULL,patt4.colnms(nsub)))
      
      # for testing: ex_t <- read_xlsx("//sassysprd/qsci/csm/dj006/myCSM/RGV_simulation/consecutive_example.xlsx", sheet="ex_t")
      # ex_w <- read_xlsx("//sassysprd/qsci/csm/dj006/myCSM/RGV_simulation/consecutive_example.xlsx", sheet="ex_w")
      cli_alert_info('tbl*.act')
      tbl1.act <- patt1(site_sig_w[,-1])
      patt1.act[1, c(as.integer(names(tbl1.act)))] <- tbl1.act
      tbl2.act <- patt2(site_sig_w[,-1])
      patt2.act[1, c(as.integer(names(tbl2.act)))] <- tbl2.act
      tbl3.act <- patt3(site_sig$CHGC)
      patt3.act[1, c(as.integer(names(tbl3.act)))] <- tbl3.act
      patt4res.act <- patt4(site_sig)
      tbl4.act <- if (nrow(patt4res.act) > 0) table(patt4res.act$n_subj) else matrix(0,ncol=1,dimnames=list(NULL,"1"))
      patt4.act[1, c(as.integer(names(tbl4.act)))] <- tbl4.act
      
      cli_alert_info('patt_red')
      pattern1 <- patt_res(1, patt1.act, sim_res[[1]])
      pattern2 <- patt_res(2, patt2.act, sim_res[[2]])
      pattern3 <- patt_res(3, patt3.act, sim_res[[3]])
      pattern4 <- patt_res(4, patt4.act, sim_res[[4]])
      
      patt1.label <- "Pattern 1: occurrence of value at same visit in # subjects"
      patt2.label <- "Pattern 2: occurrence of same value in same subject at # visits"
      patt3.label <- "Pattern 3: occurrence of same value at # visits in study"
      patt4.label <- "Pattern 4: occurrence of same value in # subjects at 2 consecutive visits"
      
      cli_alert_info('wrksheet')
      # add actual data table and patt1 - patt4 results for PARAMCD_SITEID into new worksheet
      # wb <- createWorkbook(); ws_n <- 0
      ws_n <- ws_n + 1
      addWorksheet(wb.res, sheetName=paste(gv_sites[ii], sigs_ii[jj], sep="_"))
      # freezePane(wb.res, sheet=ws_n, firstRow=TRUE)
      writeDataTable(wb.res, sheet=ws_n, x=site_sig_w, colNames=TRUE, withFilter=FALSE) # actual data
      nextrow <- nrow(site_sig_w)+3
      writeData(wb.res, sheet=ws_n, x=patt1.label, startCol=1, startRow=nextrow, colNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow+1 # ?
      writeDataTable(wb.res, sheet=ws_n, x=as.data.frame(pattern1), startCol=1, startRow=nextrow, 
                     colNames=TRUE, rowNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow + if (nrow(pattern1)==3) 5 else 3 # ?
      writeData(wb.res, sheet=ws_n, x=patt2.label, startCol=1, startRow=nextrow, colNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow+1 # ?
      writeDataTable(wb.res, sheet=ws_n, x=as.data.frame(pattern2), startCol=1, startRow=nextrow, 
                     colNames=TRUE, rowNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow + if (nrow(pattern2)==3) 5 else 3 # ?
      writeData(wb.res, sheet=ws_n, x=patt3.label, startCol=1, startRow=nextrow, colNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow+1 # ?
      writeDataTable(wb.res, sheet=ws_n, x=as.data.frame(pattern3), startCol=1, startRow=nextrow, 
                     colNames=TRUE, rowNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow + if (nrow(pattern3)==3) 5 else 3 # ?
      writeData(wb.res, sheet=ws_n, x=patt4.label, startCol=1, startRow=nextrow, colNames=FALSE, withFilter=FALSE)
      nextrow <- nextrow+1 # ?
      writeDataTable(wb.res, sheet=ws_n, x=as.data.frame(pattern4), startCol=1, startRow=nextrow, 
                     colNames=TRUE, rowNames=FALSE, withFilter=FALSE)
    } # for jj
  } # for ii

  saveWorkbook(wb.dat, path_one, overwrite=TRUE)
  saveWorkbook(wb.res, path_two, overwrite=TRUE)
  # rm(proj,ind,study,cutdt,fldr,cdm_path,rsys_path,scbd_path,scbd_files,scbd_file,gv_flagged,gv_sites,gv_data)
  # rm(ii,jj,signals_ii,sigs_ii,n_sigs,site_sig,sim_res,site_sig_w,nsub,nvis,nobs,patt1.act,patt2.act,patt3.act,patt4.act)
  # rm(tbl1.act,tbl2.act,tbl3.act,patt4res.act,tbl4.act,pattern1,pattern2,pattern3,pattern4,wb,ws_n,wb_name)
}
