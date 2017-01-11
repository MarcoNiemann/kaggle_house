#'
#' Cleaning data function
#'
#' Main purpose is to remove a set of rows and columns from a given dataset.
#'
#' After checking for valid input parameter first rows and then
#' columns are removed from the \code{data}. The cleaned dataset is globally available under
#' the name of the input dataset with "_clean" as prefix.
#'
#' @param data Input data that is supposed to be reduced in number of columns and rows
#'
#' @param rows Rows that should be removed from \code{data}
#'
#' @param cols Columns that should be removed from \code{data}
#'
#' @return \code{data} with removed rows and columns
#'
preprocess.generate_cleaned_data <- function(data, rows, cols) {
  if(length(rows) > 0 && length(cols) == 0) {
    new_data = data[-rows, ]
  } else if(length(cols) > 0 && length(rows) == 0) {
    new_data = data[ , -cols]
  } else if(length(cols) > 0 && length(rows) > 0) {
    new_data = data[-rows, -cols]
  } else {
    stop("Invalid arguments!")
  }
  assign(paste0(deparse(substitute(data)), "_clean"), new_data, envir = .GlobalEnv)
}