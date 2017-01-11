#' Data transformation function
#'
#' Substitutes missing values of a specific set of columns with the
#' character "None".
#'
#' Especially in the given dataset missing values often yielded
#' semantical meaning. To generate an additional class when transformed
#' into numerical values this function is used to substitutes those
#' missing values not at random with a character "None" that should
#' get transformed into an numeric value instead of NA.
#'
#' @param data Input data which contains missing values
#'
#' @param na.col Set of column names which contain at least one
#' missing value that is then substituted with the character
#' "None"
#'
#' @examples
#'  KaggleHouse:::prepare.transform_data(data.frame(id = c(1,2,NA)), "id")
#'
prepare.transform_data <- function(data, na.col) {
  na.free.col <- setdiff(colnames(data), na.col)

  tmp = data.frame(lapply(colnames(data), function(col) {
    dat <- data[, col]
    if(col %in% na.col) {
      if(is.factor(dat) || is.character(dat)) {
        cpp_rep_na_chr(dat, "None")
      } else {
        cpp_rep_na_num(dat, 0)
      }
    } else {
      dat
    }
  }))

  #colnames(tmp) <- na.col
  colnames(tmp) <- colnames(data)
  #tmp = cbind(tmp, data[, na.free.col])

  train_numeric = data.frame(lapply(tmp, function(col) {
    if(!is.numeric(col)) {
      as.integer(factor(col))
    } else {
      col
    }
  }))

  assign(paste0(deparse(substitute(data)), "_numeric"), train_numeric,
         envir = .GlobalEnv)
  assign(paste0(deparse(substitute(data)), "_none"), tmp, envir = .GlobalEnv)
}
