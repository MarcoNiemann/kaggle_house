#' Set Cat Function to Echo variable
#'
#' This function assigns the alias 'echo' to the cat-function.
#' 
echo <- cat

#' Write String to Output with appended Newline
#'
#' This function can be used to write a string \code{str} to the output with
#' an automatically appended newline.
#'
#' @param str with the content to print to the output
#'
echoln <- function(str) {
  echo(str, "\n", sep = "")
}

echoln <- compiler::cmpfun(echoln)

#' Remove File-Extension from Filepath
#'
#' This function takes a filepath including the extension, removes the extension
#' and returns the remaining path.
#'
#' This function accepts a filepath of the structure \code{folder/file.ext}. It
#' determines the last point in the character string via a RegEx operation. The
#' underlying logic is that most file extensions will be separated from the
#' remaining path by that point. Then only the part of the filepath up to that
#' point is extracted and kept. So the resulting filepath should look like
#' \code{folder/file}.
#'
#' @param filePath  \code{character} string representing a filepath like
#'                  \code{folder/file.ext}.
#'
#' @return  \code{character} string representing path in parameter
#'          \code{filePath} without the fileextension.
#'
#' @examples
#'  # Input is 'data.csv' and output should be 'data'.
#'  KaggleHouse:::util.remove.fileextension("data.csv")
#'
util.remove.fileextension <- function(filePath) {
  lastPointPosition <- regexpr("\\.[^\\.]*$", filePath)
  name <- substr(filePath, 0, lastPointPosition - 1)
  return(name)
}

util.remove.fileextension <- compiler::cmpfun(util.remove.fileextension)

#' Clear Matrix of all \code{NA} rows.
#'
#' Function to remove rows from a given matrix \code{m} which contain a
#' specified amount of \code{NA} values. The default is that a row is removed
#' from the matrix as soon as one \code{NA} value is included. But it is also
#' possible to configure the method to remove rows only when all values are
#' \code{NA}.
#'
#' @param x with the matrix to clear from \code{NA} values
#' @param fun with the function to determine the number of allowed \code{NA}s
#'
#' @return Matrix with cleaned rows
#'
util.na.rm <- function(x, fun = any) {
  # TODO: generalize for vectors
  stopifnot(is.matrix(x) || is.data.frame(x))
  i <- apply(x, 1, function(y) fun(is.na(y)))
  x[ !i, ]
}

util.na.rm <- compiler::cmpfun(util.na.rm)

#' Define a Scatterplot Panel to Show Correlation Coefficients
#'
#' This function calculates the correlation between two datasets \code{x} and
#' \code{y} and writes the textual representation into the corresponding field
#' of the scatterplot panel.
#' Depending on the derived value different font sizes and color schemes are
#' applied.
#'
#' @param x first dataset
#' @param y second dataset
#' @param digits number of digits after the decimal separator
#' @param cex.cor desired font/text size
#' @param ... further arguments
#'
util.panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  # Save the "usr" graphical parameters to reset it at this function 's
  # termination.
  usr <- par("usr"); on.exit(par(usr))

  # Define the extremes of the user coordinates of the plotting region.
  par(usr = c(0, 1, 0, 1))

  # Calculate the correlations after removing NA values and round the derived
  # values to the desired number of digits.
  r <- abs(cor(matrix(util.na.rm(cbind(x, y)), ncol = 2))[1, 2])
  txt <- format(r, digits = digits, nsmall = digits)

  # Set a standard font/text size, when it has not been assigned.
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)

  # Set the standard color to grey.
  col = "grey"
  # If Correlation calculation fails due to NA values the color is set to blue.
  # In case the correlation values are very high (>0.8) the color is set to red.
  if (is.na(r)) col = col.ercis.cyan
  else if (r > 0.8) col = col.ercis.red

  # The text is printed to the middle of the scatterplot element describing the
  # correlation between two values (where the respective column and row cross).
  text(0.5, 0.5, txt, cex = cex.cor * r, col = col)
}

util.panel.cor <- compiler::cmpfun(util.panel.cor)

#' Write data to a CSV-file.
#'
#' Wrapper function around \code{write.table} that writes a given set of data
#' to a file with the name \code{name}. Predefined separator is \code{;} and the
#' used file-encoding is \code{utf-8}.
#'
#' @param data being a matrix or data-frame containing the writable data
#' @param name being a string providing the filename
#' @param ... additional arguments to write.csv2
#'
util.write_csv <- function(data, name, ...) {
  stopifnot(is.matrix(data) || is.data.frame(data))
  write.csv2(data, file = name, fileEncoding = "utf-8", ...)
}

util.write_csv <- compiler::cmpfun(util.write_csv)

#' Convert list of matrices to matrix
#'
#' Transforms a list of matrices (with the same columns ['objects', 'counts'])
#' into a concatenated matrix.
#'
#' @param lst \code{list} with matrices.
#'
#' @return \code{matrix} being the concatenated version of all matrices in
#'         \code{list} \code{lst}.
#'
util.list_to_matrix <- function(lst) {
  # prevent the function from being executed with a non-list object
  stopifnot(is.list(lst))
  cbind(
    unlist(lapply(lst, function(m) m[,1])),
    unlist(lapply(lst, function(m) m[,2]))
  )
}

util.list_to_matrix <- compiler::cmpfun(util.list_to_matrix)

#' Convert List of Matrices to \code{data.frame}
#'
#' Transforms a list of matrices (with the same columns ['objects', 'counts'])
#' into a concatenated \code{data.frame}.
#'
#' This method first uses the \code{\link{util.list_to_matrix}} method to
#' convert the given list of matrices to the concatenated version of all
#' matrices stored in a single matrix. Afterwards the derived matrix is
#' transformed into the \code{data.frame} returned by the function.
#'
#' @param lst List with matrices.
#'
#' @return \code{data.frame} being the concatenated version of all matrices in
#'         \code{list} \code{lst}.
#'
util.list_to_df <- function(lst) {
  mat <- util.list_to_matrix(lst)
  return(as.data.frame(mat))
}

util.list_to_df <- compiler::cmpfun(util.list_to_df)

#' Ensure Existance of Directory
#'
#' This method ensures that the directory given as a parameter exists.
#'
#' This method takes a \code{character} string as input which denotes a
#' directory path. It first checks whether the given directory already exists
#' on the filesystem. If this is not the case it will be created.
#'
#' @param dir.name  \code{character} string of the name of the directory which
#'                  should be checked for existance and added if missing.
#'
#' @examples
#'  KaggleHouse:::util.ensure.dir("output")
util.ensure.dir <- function(dir.name) {
  if (!file.exists(dir.name)) {
    dir.create(dir.name)
  }
}

util.ensure.dir <- compiler::cmpfun(util.ensure.dir)

#' Check Data Columns for \code{NA} Values
#'
#' This method returns a \code{logical} vector that indicates the presence of
#' \code{NA} values in every column of \code{data}.
#'
#' This method takes a \code{data.frame} as an input and then checks for each
#' column if it contains any \code{NA} values. The results get stored in a
#' \code{logical} vector (always together with the column name).
#'
#' @param data  \code{data.frame} in which each column should be checked for the
#'              presence of \code{NA} values.
#'
#' @return \code{Named logi} (being a \code{logical} vector also containing the
#'         column name for each \code{logical} value) indicating the presence
#'         of \code{NA} values for each column of \code{data}.
#'
#' @examples
#'  KaggleHouse:::util.contain_na(data_train)
#'
util.contain_na <- function(data) {
  na.col = apply(data, 2, function(col) {
    any(is.na(col))
  })
  na.col
}

util.contain_na <- compiler::cmpfun(util.contain_na)

#' Count Number of \code{NA} Values in a \code{data.frame}
#'
#' This method counts the number of \code{NA} values for each column of a given
#' \code{data.frame}.
#'
#' This method takes a \code{data.frame} as an input and then counts the number
#' of \code{NA} values in each column. The results get stored in a
#' \code{numeric} vector (always together with the column name).
#'
#' @param data  \code{data.frame} in which the number of \code{NA} values is
#'              counted for each column.
#'
#' @return \code{Named num} (being a \code{numeric} vector also containing the
#'         column name for each \code{numeric} value) indicating the number of
#'         \code{NA} values for each column in \code{data}.
#' @examples
#'  KaggleHouse:::util.number_na(data_train)
#'
util.number_na <- function(data) {
  colSums(sapply(data, is.na))
}

util.number_na <- compiler::cmpfun(util.number_na)

#' Generate a Kaggle-submitable CSV from the Predicted Data
#'
#' This method generates a CSV file submitable to Kaggle from a
#' \code{data.frame} containing the relevant data (incl. the predictions).
#'
#' This method takes a \code{data.frame} that already contains all relevant
#' data for a prediction submission to the Kaggle platform. E.g. in case of the
#' "House Prices: Advanced Regression Techniques" challenge this
#' \code{data.frame} would have to include one column with the ID and one with
#' the predicted sale price of a house.
#' These data is then written to a CSV file via a preconfigured call to the
#' \code{\link{write.csv}} function to a file with the name given in the
#' \code{name} parameter.
#'
#' @param data  \code{data.frame} containing the data needed for a submission of
#'              predictions to the Kaggle platform.
#' @param name  \code{character} string that represents the name of the CSV file
#'              to which the submission data will be stored.
#'
util.generate.submit <- function(data, name) {
  write.csv(data, file = name, row.names = FALSE, qmethod = "double",
            quote = FALSE)
}

util.generate.submit <- compiler::cmpfun(util.generate.submit)

#' Compute the MSE between two \code{data.frame}s
#'
#' This method is used to compute the mean squared error (MSE) between two
#' \code{data.frame}s.
#'
#' @param data_predicted  First \code{data.frame} for the MSE computation.
#' @param data_original   Second \code{data.frame} for the MSE computation.
util.mse <- function(data_predicted, data_original) {
  mean((data_predicted - data_original)^2)
}

util.mse <- compiler::cmpfun(util.mse)
