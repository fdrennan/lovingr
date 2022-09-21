#' get_scoreboard
#' @export get_scoreboard
get_scoreboard <- function(path) {
  map(ls(), ~ print(get(.)))
  scbd <- read_xlsx(path, sheet = "Scoreboard") %>%
    filter(potential_issue %in% c("identical gv values", "repeated vital sign values"))
  if (nrow(scbd) == 0) {
    return(NULL)
  } else {
    names(scbd)[8] <- "summary_stats"
    return(scbd[, c(2, 3, 4, 7, 8)])
  }
}

# an overlaid by-subject line plot
#' gv_plot
#' @export gv_plot
gv_plot <- function(gvdat, siteid, country, paramcd, param) {
  map(ls(), ~ print(get(.)))
  cli_alert_info("Executing gv_plot")
  # gvdat <- gv_data; siteid <- gv_sites[ii]; country <- gv_flagged$country[ii]
  # paramcd <- sigs_ii[jj]; param <- gv_paramcd$PARAM[which(gv_paramcd$PARAMCD==sigs_ii[jj])]
  data_temp <- gvdat[gvdat$siteid == siteid & gvdat$paramcd == tolower(paramcd) & (!is.na(gvdat$aval)) &
    (grepl("week", tolower(gvdat$visit)) | gvdat$visitnum == 0), ]
  data_temp$weekn <- as.integer(data_temp$visitnum)

  # the following is a line plot of each subject's data (does not start at 0). It is impossible to
  # tell where the duplicate differences are.
  g <- ggplot(data_temp, aes(weekn, aval, group = subject, color = subject)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none") +
    geom_dl(aes(label = subject), method = list(dl.trans(x = x - 1), "last.points", cex = 0.7)) +
    # geom_dl(aes(label = SUBJECT), method = list(dl.trans(x = x - 0.1), "first.bumpup", cex = 0.7)) +
    ggtitle(paste(" Site ", siteid, " (", toupper(country), ")\n", param, sep = "")) +
    xlab("Week") +
    ylab(param) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(
      limits = c(0, max(data_temp$weekn)),
      breaks = unique(data_temp$weekn)
    ) # seq(0, max(data_temp$weekn)+26, by = 13))
  # print(g)

  # Better idea: line plot of differences over time for each subject. Only include dup values?
  # Number of dups (from table) determines size of dot?
  return(g)
}

#' gvdups_tbl
#' @export gvdups_tbl
gvdups_tbl <- function(gvdat, siteid, paramcd) {
  map(ls(), ~ print(get(.)))
  cli_alert_info("Executing gvdups_tbl")
  # gvdat <- gv_data; siteid <- gv_sites[ii]; paramcd <- sigs_ii[jj]
  # gvdat <- gv_data; siteid <- "0028"; paramcd <- "ARMSP"
  data_temp <- gvdat[which(gvdat$siteid == siteid & gvdat$PARAMCD == paramcd & (!is.na(gvdat$CHG)) &
    grepl("WEEK", toupper(gvdat$VISIT))), ]

  dup_CHGC <- as.data.frame(table(data_temp$CHGC[abs(data_temp$CHG) > 0.001])) %>%
    mutate(CHG = as.numeric(levels(Var1)), CHGC = levels(Var1)) %>%
    select(CHG, CHGC, Freq) %>%
    filter(Freq > 1) %>%
    arrange(CHG, CHGC)

  # create a dupCHGC * WK dataset with SUBJID as cell values
  data_temp2 <- data_temp %>%
    mutate(
      SUBJID = ifelse(CHGC %in% dup_CHGC$CHGC, substr(SUBJECT, 6, 9), NA),
      WK = paste0("W", ifelse(nchar(VISITNUM) == 2, paste0("0", VISITNUM), VISITNUM))
    ) %>%
    select(SUBJID, WK, CHGC) %>%
    filter(!is.na(SUBJID))

  dup_CHGC_WK <- as.data.frame(table(data_temp2$WK, data_temp2$CHGC)) %>%
    mutate(CHGC = as.character(Var2), WK = as.character(Var1)) %>%
    filter(Freq > 0) %>%
    select(CHGC, WK, Freq) # Freq = nbr SUBJIDs at given CHGC/WK
  data_temp2freq <- as.data.frame(left_join(data_temp2, dup_CHGC_WK, by = c("CHGC" = "CHGC", "WK" = "WK"))) %>%
    arrange(CHGC, WK, SUBJID)
  data_temp2Freq1 <- filter(data_temp2freq, Freq == 1)
  data_temp2Freq2 <- filter(data_temp2freq, Freq > 1)
  uniq_WK_CHGC <- unique(data_temp2Freq2[, 2:3])
  # create strings of those subjects which will occupy the dupCHGC*WK cell in the output Excel file
  for (ii in 1:nrow(uniq_WK_CHGC)) { # ii <- 0;   ii <- ii+1
    uniq_WK_CHGC$SUBJID[ii] <- paste(data_temp2Freq2$SUBJID[which((data_temp2Freq2$WK == uniq_WK_CHGC$WK[ii]) &
      (data_temp2Freq2$CHGC == uniq_WK_CHGC$CHGC[ii]))],
    collapse = ", \n"
    )
  }

  rgvdups_w <- rbind(data_temp2Freq1[, -4], uniq_WK_CHGC) %>%
    arrange(CHGC, WK) %>%
    # select(CHGC,WK,SUBJID) %>%
    pivot_wider(names_from = WK, values_from = SUBJID)
  rgvdups_w <- as.data.frame(left_join(rgvdups_w, dup_CHGC[, 2:3], by = c("CHGC" = "CHGC"))) %>%
    mutate(CHG = as.numeric(CHGC)) %>%
    arrange(CHG) %>%
    select(!CHG)
  col_order <- sort(names(rgvdups_w))
  rgvdups_w <- rgvdups_w[, c(col_order)]
  return(rgvdups_w)
}

# histogram of VS for site (blue) overlaid on histogram of VS for study (red)
#' vs_plot
#' @export vs_plot
vs_plot <- function(vsdat, siteid, country, this_sig, vsparamcd, stdy, outpath) {
  # map(ls(), ~ print(get(.)))
  cli_alert_info("Executing vs_plot")
  dv_param <- vsparamcd$dv_param[which(vsparamcd$dv_param == this_sig)]
  sig_dat <- vsdat[
    which(vsdat$dv_paramcd == this_sig & !is.na(vsdat$aval)),
    c("siteid", "aval")
  ]

  xl <- min(sig_dat$aval)
  xu <- max(sig_dat$aval)
  xr <- xu - xl
  if (regexpr("temp", this_sig) < 0) {
    label_by <- ceiling(xr / 10)
    bar_width <- 1
  } else {
    label_by <- 0.5
    bar_width <- 0.1
  }
  breaks <- seq(floor(xl - xr / 10), ceiling(xu + xr / 10), by = label_by)
  if (length(breaks) > 15) angle <- 315 else angle <- 0
  legend_colors <- c(Study = "red", Site = "blue")

  g <- ggplot(sig_dat, aes(x = aval, y = (..count..) / sum(..count..) * 100)) +
    geom_bar(data = sig_dat, aes(fill = "Study", group = aval), width = 0.8 * bar_width) +
    geom_bar(data = sig_dat[sig_dat$siteid == siteid, ], aes(fill = "Site", group = aval), width = 0.3 * bar_width) +
    ggtitle(paste(stdy, " Site ", siteid, " (", toupper(country), ")\n", dv_param, sep = "")) +
    labs(x = dv_param, y = "%") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = c(0.9, 0.9)) +
    scale_x_continuous(breaks = breaks) +
    scale_fill_manual("Legend", values = c(Study = "red", Site = "blue")) +
    theme(axis.text.x = element_text(angle = angle, vjust = 0.5, hjust = 0.5))

  filepath <- paste0(c(siteid, country, this_sig, stdy), collapse = "-")
  filepath <- file.path(outpath, filepath)
  path_ext(filepath) <- ".jpg"
  cli_alert_info("Saving VS plot to {filepath}")
  ggsave(filepath)
  return(g)
}

#' gv_plots
#' @export gv_plots
gv_plots <- function(scbd, dups_tbl = TRUE, outpath, data_path) {
  gv_flagged <- filter(scbd, potential_issue == "identical gv values")

  if (nrow(gv_flagged) == 0) {
    return(NULL)
  }

  gv_sites <- gv_flagged$site
  gv_signals <- gsub("\n", ", ", gv_flagged$summary_stats)
  gv_sigs_v <- tolower(unlist(str_split(paste(gv_signals, collapse = ", "), ", ")))
  gv_sigs <- unique(substr(gv_sigs_v, 1, regexpr("-", gv_sigs_v) - 2))

  gv_data <- csm_data_load(data_path) %>%
    select(subject, siteid, country, visit, visitnum, param, paramcd, aval, chg, chgc)
  gv_paramcd <- unique(gv_data[, 7:6])

  if (dups_tbl) {
    wb <- createWorkbook()
    ws_n <- 0
  }

  for (ii in 1:length(gv_sites)) {
    signals_ii <- gsub("\n", ", ", gv_flagged$summary_stats[ii])
    sigs_v_ii <- toupper(unlist(str_split(paste(signals_ii, collapse = ", "), ", ")))
    sigs_ii <- unique(substr(sigs_v_ii, 1, regexpr("-", sigs_v_ii) - 2))
    n_sigs <- length(sigs_ii)
    for (jj in 1:n_sigs) { # jj <- 1     jj <- jj+1
      g <- gv_plot(
        gv_data, gv_sites[ii], gv_flagged$country[ii], sigs_ii[jj],
        gv_paramcd$param[which(gv_paramcd$paramcd == sigs_ii[jj])]
      )

      filepath <- paste0(
        c(
          gv_sites[ii],
          gv_flagged$country[ii],
          gv_paramcd$paramcd[which(gv_paramcd$paramcd == tolower(sigs_ii[jj]))]
        ),
        collapse = "-"
      )

      filepath <- file.path(outpath, filepath)
      path_ext(filepath) <- ".jpg"
      cli_alert_info("Saving GV plot to {filepath}")
      ggsave(filepath)
      if (dups_tbl) {
        ws_n <- ws_n + 1
        tbl <- gvdups_tbl(gv_data, gv_sites[ii], sigs_ii[jj])
        addWorksheet(wb, sheetName = paste(gv_sites[ii], sigs_ii[jj], sep = "_"))
        freezePane(wb, sheet = ws_n, firstRow = TRUE)
        writeDataTable(wb, sheet = ws_n, x = tbl, colNames = TRUE)
      }
    }
  }

  if (dups_tbl) {
    saveWorkbook(wb, paste0(rsys_path, "/output/RGV/", study, "_", cutoff, "_GVdups_flagged.xlsx"),
      overwrite = TRUE
    )
  }
}

#' vs_plots
#' @export vs_plots
vs_plots <- function(scbd, outpath, study = "missing", data_path) {
  vs_flagged <- filter(scbd, potential_issue == "repeated vital sign values")

  if (nrow(vs_flagged) != 0) {
    vs_sites <- vs_flagged$site
    vs_signals <- gsub("\n", ", ", vs_flagged$summary_stats)
    vs_sigs_v <- toupper(unlist(str_split(paste(vs_signals, collapse = ", "), ", ")))
    vs_sigs <- unique(substr(vs_sigs_v, 1, regexpr(" ", vs_sigs_v) - 1))
    if (any(regexpr("sysdia", vs_sigs) > 0)) vs_sigs <- vs_sigs[-which(regexpr("sysdia", vs_sigs) > 0)]
    vs_data <- csm_data_load(data_path) %>%
      rename(visitnum = visitseq) %>%
      filter(paramcd != "sysdia") %>% # cannot make figure of SYSDIA
      mutate(
        aval = as.numeric(avalc), # DV=toupper(VSDV),
        dv_param = ifelse(paramcd %in% c("sysbp", "diabp"), paste(vsdv, param, sep = " "), param),
        dv_paramcd = ifelse(paramcd %in% c("sysbp", "diabp"),
          paste(tolower(vsdv), paramcd, sep = "_"), paramcd
        )
      ) %>%
      select(subject, siteid, country, visit, visitnum, vsdv, param, paramcd, dv_param, dv_paramcd, aval) %>%
      filter(paramcd != "") # View(vs_data)
    vs_paramcd <- unique(vs_data[, 6:10])
    # outpath <- paste0(rsys_path, "/output/VITALS/", study, "_VS_", cutoff, "_flagged.pdf")


    for (ii in 1:length(vs_sites)) { # ii <- 0;    ii <- ii+1
      signals_ii <- gsub("\n", ", ", vs_flagged$summary_stats[ii])
      sigs_v_ii <- tolower(unlist(str_split(paste(signals_ii, collapse = ", "), ", ")))
      sigs_ii <- unique(substr(sigs_v_ii, 1, regexpr(" ", sigs_v_ii) - 1))
      if (any(regexpr("sysdia", sigs_ii) > 0)) sigs_ii <- sigs_ii[-which(regexpr("sysdia", sigs_ii) > 0)] # delete SYSDIA
      n_sigs <- length(sigs_ii)
      if (n_sigs == 0) {
        next
      } else { # if n_sigs==0 then all summary_stats for site were SYSDIA -> next ii (next site)
        for (jj in 1:n_sigs) { # jj <- 0;   jj <- jj+1

          g <- vs_plot(
            vs_data, vs_sites[ii], vs_flagged$country[ii],
            sigs_ii[jj], vs_paramcd, study, outpath
          )
        }
      }
    }
  }
}
