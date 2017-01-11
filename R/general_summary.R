#' Data Summary - Get a Summary of the Data
#'
#' This function executes the \code{summary}-function on inputted datasets
#' \code{x}. The results are written to a csv-file for later use.
#'
#' @param x dataset to be analysed
general_summary_detail <- function(x, name) {
  file <- paste0(dir_presentation, "/understand_data.summary_", name, ".csv")
  results <- summary(x)
  results <- as.matrix(results)
  util.write_csv(results, file)
}

general_summary_detail <- compiler::cmpfun(general_summary_detail)

general_summary_detail_df <- function(df) {
  stopifnot(is.data.frame(df))
  sapply(colnames(df), function(x) {
    general_summary_detail(df[,x], x)
  })
}

general_summary_detail_df <- compiler::cmpfun(general_summary_detail_df)

general_summary_df <- function(df) {
  echoln("Generate Summary Textdocument...")
  stopifnot(is.data.frame(df))
  sink(paste0(dir_presentation, "/summary/data_summary.txt"))
  sapply(colnames(df), function(x) {
    echoln(x)
    print(summary(df[,x]))
    echoln("")
  })
  sink()
  echoln("Generation of Summary Textdocument finished.")
}
