#' Replace multiple strings across multiple files with original values and replacement values.
#'
#' files - A character string of file names to work through.
#' f - A character string of original values that you want to replace.
#' r - A character string of replacement values (f->r).
#' @export multi_replace
multi_replace <- function(files = "", f = "", r = "") {
  file_line <- data.frame() # (optional) tracking table
  # loop through each file separately
  for (j in 1:length(files)) {
    nl <- suppressWarnings(readLines(files[j])) # read file line by line
    # loop through each of the find and replace values within each file
    for (i in 1:length(f)) {
      cnt_replaced <- data.frame(filename = files[j], find = f[i], replace = r[i], times = length(grep(f[i], nl))) # fill tracking table with values
      file_line <- rbind(file_line, cnt_replaced) # populate tracking table count of find & replace within each file
      nl <- gsub(f[i], r[i], nl) # find and replace value line by line
    }
    write(nl, file = files[j]) # save files with same name & overwrite old
    rm(nl) # don't overwrite with previous file if error.
  }
  return(file_line) # print the tracking table
}
