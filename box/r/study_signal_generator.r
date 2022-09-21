#' study_signal_generator
#'
#' @description Given a properly configured Excel file, study_signal_generator
#' generates all of the required output to complete a scoreboard of the csm
#' analyses.
#'
#' I've had a difficult time trying to determine where to begin documenting.
#' I think that trying to describe from a brief theoretical perspective is an
#' excellent way to start.
#'
#'
#' ## Purpose:
#'
#' The purpose of the csm package is to identify an event that satisfies a
#' particular set of criteria. An event is simply a named, one-dimensional
#' vector we call a signal. For example, `c(analysis = 'ae', p_value = .3, n_subj = 3)` is a signal
#' vector that contains dummy information from an analysis.  An analysis is a
#' table  of signals (HR, toxgr1, saecnt) collected from multiple sites which
#' are typically compared to one another and then staged for flagging.
#'
#' ### How flags are applied:
#'
#' The biostatistican supplies code to R in the form of an R statement.
#'
#' For example, if we see the following statement for an ae analysis,
#' the the vector above would receive a flag of 1.
#'
#' `if (p_value < .05, n_subj > 2) {flag = 1}`
#'
#' It is important to know that this operation can only be performed row-wise.
#' It is impossible to generate statistics such as the average p_value of a
#' particular group, not to say that one would do this, but only to point out
#' that the easiest way to explain the concept and why above I called the
#' signals 'individually flagged.'
#'
#' #### Note:
#'
#' As the R Ecosystem at BioMarin matures, it would be wise to consider
#' converting our Excel spreadsheet into a more dynamic Shiny application.
#' Obtaining more control of R's interface would allow for 'point and click'
#' signal selection and numeric constraints (rules that are difficult to
#' enforce from Excel). A Shiny interface would also be a more integrated
#' platform instead of using Excel RStudio simultaneously.
#'
#' ### Dummy Walkthrough:
#'
#' Consider the table below. Note that the values below are completely
#' arbitrary, but will allow us to quickly walk through a dummy flagging
#' procedure.
#'
#' Suppose we are provided the following from a SAS dataset.
#'
#' ```
#' |analysis | signal | n | r  | site |
#' |---------|--------|---|----|------|
#' |aei      | toxgr1 | 4 | 6  |0001  |
#' |aei      | toxgr1 | 5 | 6  |0002  |
#' |aei      | toxgr2 | 1 | 6  |1234  |
#' |aei      | toxgr2 | 5 | 6  |1234  |
#' ```
#'
#' We split the analysis data on the signal column and create a list of
#' data.frames by signal type (toxgr1 and toxgr2). A list of data.frames makes
#' it very easy to create statistics for signals that are unknown. If the data
#' is in a columnar format - with signal names assigned to the names of the
#' table columns - it makes it difficult to anticipate which columns should be
#' grouped together without having to create a programming process for
#' selecting and parsing column names.
#'
#' The data above is split like so:
#'
#' ```
#' |analysis | signal | n | r  | site |
#' |---------|--------|---|----|------|
#' |aei      | toxgr1 | 4 | 6  |0001  |
#' |aei      | toxgr1 | 5 | 6  |0002  |
#'
#' |analysis | signal | n | r  | site |
#' |---------|--------|---|----|------|
#' |aei      | toxgr2 | 1 | 6  |1234  |
#' |aei      | toxgr2 | 5 | 6  |1234  |
#' ```
#'
#' Now for each data.frame, some statistic is created (columns `p` and `s``).
#'
#' ```
#' |analysis | signal | p | s   | site |
#' |---------|--------|---|-----|------|
#' |aei      | toxgr1 | 2 | 12  |0001  |
#' |aei      | toxgr1 | 2 | 12  |0002  |
#'
#' |analysis | signal | p  | s   | site |
#' |---------|--------|----|-----|------|
#' |aei      | toxgr2 | 3  | 12  |1234  |
#' |aei      | toxgr2 | 10 | 12  |1234  |
#' ```
#'
#' Suppose in our Excel file, we specify that for toxgr1 if we
#' get `p` = 2 then we will flag the site. For toxgr2, we will say
#' if `p` = 10, then we will flag the site.
#'
#' The flagging operation is currently completed in a row-wise fashion.
#'
#' ```
#' |analysis | signal | p | s   | site | flag |
#' |---------|--------|---|-----|------|------|
#' |aei      | toxgr1 | 2 | 12  |0001  |1     |
#'
#'
#' |analysis | signal | p | s   | site | flag |
#' |---------|--------|---|-----|------|------|
#' |aei      | toxgr1 | 2 | 12  |0002  |1     |
#'
#'
#' |analysis | signal | p  | s   | site | flag |
#' |---------|--------|----|-----|------|------|
#' |aei      | toxgr2 | 3  | 12  |1234  |0     |
#'
#' |analysis | signal | p  | s    | site | flag |
#' |---------|--------|----|------|------|------|
#' |aei      | toxgr2 | 10 | 12   |1234  |1     |
#' ```
#'
#' As we can see, sites `0001` and `002` are flagged because `p` = 2 (for
#' `toxgr1`), and site `1234` is flagged because `p` = 10 (for `toxgr2`)
#'
#' ### What is an Analysis?
#'
#' An analysis is an umbrella term for a collection of signals that are analyzed
#' in a particular way. For example, should we use the CompareProportion function
#' or the CompareMean function? The analysis type determines the type of statistical
#' analyses are completed. The signal or paramcd (param code) groups data
#' under an analysis and each item in that group is typically compared to
#' the group as a whole. In the table we see aei and vitals. These are two
#' actual analyzes that we complete which have their own paramcd values.
#'
#' We then split our data into smaller, signal level chunks. Note that it is
#' expected that there are multiple sites for each group of analysis and signal.
#' Note that this splitting process is completed before the generation of say, the
#' p_value displayed in the table below.
#'
#' ```
#' |analysis | signal | p_value | n | r  | site |
#' |---------|--------|---------|---|----|------|
#' |aei      | toxgr1 | .44     | 5 | 6  |0001  |
#' |aei      | toxgr1 | .04     | 4 | 6  |9999  |
#'
#' |analysis | signal | p_value | n | r  | site |
#' |---------|--------|---------|---|----|------|
#' |aei      | toxgr2 | .32     | 1 | 6  |0001  |
#' |aei      | toxgr2 | .04     | 4 | 6  |9999  |
#'
#' |analysis | signal | p_value | n | r  | site |
#' |---------|--------|---------|---|----|------|
#' |vitals   | hr     | .9      | 2 | 10 |0001  |
#' |vitals   | hr     | .2      | 1 | 10 |0002  |
#' |vitals   | hr     | .0      | 4 | 10 |0003  |
#' ```
#'
#' Each group is collectively analyzed, and then the Biostatistican can flag
#' sites that are abnormal according to his or her judgement. As noted above,
#' this code is applied to a dataset in a row-wise fashion.  For example,
#' applying the code `if (p_value == .44) {flag = 1}`
#' would create the following dataframe.
#'
#' ```
#' |analysis | signal | p_value | n | r  | site | flag |
#' |---------|--------|---------|---|----|------|------|
#' |aei      | toxgr1 | .44     | 5 | 6  |0001  | 1    |
#' ```
#'
#' @family study_signal_generator
#' @family main
#'
#' @seealso csm_analysis csm_mastersheet_generate build_scoreboard
#'
#' @inheritParams roxygen2::roxygenise
#' @export study_signal_generator
#' @md
#'
#' @param path_csm_excel The path to a properly configured Excel Mastersheet
#' @param debugging Whether or not to stop the analysis at natural checkpoints in the function
#' @param analysis_filter A string vector, defaults to NULL. Example,
#' @param testing Whether or not to return the analysis results from the scoreboard, defaults to FALSE.
#' @param test_input Pass the output of this function, when testing = TRUE to this input to bypass statistic generation.
study_signal_generator <- function(path_csm_excel = "CSMConfigMastersheet.xlsx",
                                   debugging = FALSE,
                                   analysis_filter = NULL,
                                   repository = NULL,
                                   notify = TRUE,
                                   input_directory = NULL,
                                   rgv_tables = NULL) {
  mastersheet <- csm_mastersheet_generate(excel_path = path_csm_excel)

  if (!is.null(input_directory)) {
    mastersheet$data_paths$final <- paste0(input_directory, "/", mastersheet$data_paths$filename)
  }

  verify_path_integrity(mastersheet, analysis_filter)

  c(meta_data, n_sub_information) %<-% grab_meta_data(mastersheet)

  if (rgv_tables) {
    # browser()
    base_path <- mastersheet$data_paths[[2]][[1]]
    rgv_input <- str_split(str_remove(base_path, '/sassys/cdm/cdmdev/'), "/")[[1]]
    
    project <- rgv_input[[1]]
    study <- rgv_input[[3]]
    cutoff <- paste0(str_extract_all(rgv_input, '[0-9]')[[4]], collapse = '')
    indication <- rgv_input[[2]]
    
    csm_rgv_tbls(project, indication, study, cutoff)
  }
  
  
  
  analysis_results <- csm_analysis(
    mastersheet = mastersheet,
    debugging = debugging,
    analysis_filter = analysis_filter
  )
  c(analysis_results, analysis_errors) %<-% separate_analysis_errors(analysis_results)
  analysis_results <- remap_rename(analysis_results, meta_data, n_sub_information)
  
  # browser()
  # Scoreboard Generation ---------------------------------------------------
  scoreboard <- tryCatch(expr = {
    build_scoreboard(
      analysis_results,
      mastersheet$scoreboard_setup,
      n_sub_information,
      notify
    )
  }, error = function(err) {
    cli_alert_danger(as.character(err))
    stop("No Scoreboard Generated - no flags.")
  })

  # Export ------------------------------------------------------------------
  outpath <- file.path(repository, "SCOREBOARD")
  dir_create(repository, recurse = TRUE, mode = "0777")

  if (is.null(scoreboard$scoreboard)) {
    cli_alert_warning("No results returned")
    return(FALSE)
  }

  write_excel_file(analysis_results, scoreboard$scoreboard, repository, "SCOREBOARD")

  response <- list(
    analysis_results = analysis_results,
    scoreboard = scoreboard$analysis_results,
    scoreboard_raw = scoreboard$analysis_results_raw
  )

  store_csv_file(response, repository)

  tryCatch(expr = {
    write_plot_files(mastersheet, repository = repository)
  }, error = function(err) {
    cli_alert_warning('Plots did not generate')
  })

  cli_alert_info("All done!")

  response
}
