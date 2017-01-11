################################################################################
#                   LOADING DATA AND STORE AS RDA                              #
################################################################################

#' Ensure Existance of Data Directory
#'
#' The function checks the existance of the \code{data} directory and creates it
#' in case it is necessary.
#'
#' The function ensures the presence of a \code{data} directory via the utility
#' helper function \code{\link{util.ensure.dir}}. This method then checks the
#' availability of the folder and creates a new folder in case it should be
#' necessary.
#'
#' @seealso \code{\link{util.ensure.dir}}
rda.conversion.ensureDataDir <- function() {
  util.ensure.dir("data")
}

rda.conversion.ensureDataDir <- compiler::cmpfun(rda.conversion.ensureDataDir)


#' Get List of RAW data files.
#'
#' The function checks the given \code{directory} for RAW data files.
#'
#' First the presented \code{directory} is checked for existance. In case it
#' cannot be found a \code{warning} will be thrown. If it is present, a list of
#' all files in the directory will be created and returned.
#' These files will then be considered as candidates for being loaded as
#' package data.
#'
#' @param directory The directory in which the RAW files should be searched.
#'
#' @return \code{List} of files present in the specified \code{directory}.
rda.conversion.checkRawData <- function(directory = "inst/extdata") {
  if (file.exists(directory)) {
    listOfFiles <- list.files(directory)
  } else {
    warning(paste("Specified raw data directory does not exist: ", directory))
  }

  return(listOfFiles)
}

rda.conversion.checkRawData <- compiler::cmpfun(rda.conversion.checkRawData)


#' Load data from RAW data file
#'
#' A function that loads raw data from a CSV file to a local variable.
#'
#' The function first checks the existance of the given \code{directory} and
#' \code{file}. In case both exist, the specified \code{file} will be loaded
#' considering the loading parameters \code{header, sep, dec} and \code{quote}.
#'
#' @param directory The directory in which the target \code{file} is located.
#' @param file  The file that should be loaded by this function.
#' @param header  \code{boolean} specifying whether the to-read-CSV has a header
#'                row or not. Defaults to \code{TRUE}.
#' @param sep Character specifying the \code{chr} separating two data entries.
#'            Defaults to \code{,}.
#' @param dec Character specifying the decimal split \code{chr}. Defaults to
#'            \code{.}.
#' @param quote Set of quoting characters. Defaults to \code{""}.
#'
#' @return \code{data.frame} containing the data present in the specified
#'          \code{file}.
#'
#' @examples
#'  # Assuming the existance of directory 'dir' and file 'data.csv':
#'  KaggleHouse:::rda.conversion.loadFile(
#'      'dir', 'data.csv', header = T, sep = ";", dec = ".", quote = '\"'
#'  )
rda.conversion.loadDataFile <- function(
  directory, file, header = T, sep = ",", dec = ".", quote = "\""
) {
  filePath <- file.path(directory, file)
  if (file.exists(filePath)) {
    dat <- read.csv(filePath, header = header, sep = sep, dec = dec,
                    quote = quote)
  } else {
    warning(paste("Specified file at location", filePath, "does not exist!"))
  }

  return(dat)
}

rda.conversion.loadDataFile <- compiler::cmpfun(rda.conversion.loadDataFile)

#' Convert Loaded Data to RDA File.
#'
#' This function converts a loaded dataset \code{dat} into an RDA file.
#'
#' This function takes a dataset \code{dat} (typically previously loaded via
#' \code{\link{rda.conversion.loadDataFile}}) and the \code{name} it comes with.
#' Based on this it first checks the \code{data} directory for an RDA file with
#' the same name (indicating that the data in question has already been
#' converted). Whenever conversion is still required, the input data \code{dat}
#' will be bound to the \code{.GlobalEnv} under the label of \code{name}. This
#' allows to store the variable content to an RDA file via
#' \code{\link{base::save}}. Finally \code{.GlobalEnv} will be cleaned from
#' \code{dat} again.
#'
#' @param dat \code{data.frame} containing the data loaded from a raw data/CSV-
#'            file.
#' @param name  Name assigned to \code{dat} by the filename of it's raw file.
#'
#' @seealso \code{\link{rda.conversion.loadDataFile}}
#'
#' @examples
#' # Assuming the existance of directory 'inst/extdata' and file 'data.csv':
#' imp <- KaggleHouse:::rda.conversion.loadDataFile('inst/extdata', 'data.csv')
#' KaggleHouse:::rda.saveAsRDA(imp, 'data.csv')
rda.conversion.saveAsRDA <- function(dat, name) {
  name <- util.remove.fileextension(name)
  if (!file.exists(file.path("data", paste0(name, ".rda")))) {
    assign(name, value = dat, envir = .GlobalEnv)
    save(list = c(name), file = file.path("data", paste0(name, ".rda")))
    rm(list = c(name), pos = .GlobalEnv)
  } else {
    warning(paste("The file", name, "already exists!"))
  }
}

rda.conversion.saveAsRDA <- compiler::cmpfun(rda.conversion.saveAsRDA)


#' Convert RAW data to RDA data.
#'
#' This function converts RAW data into the package-typical RDA format.
#'
#' This function is a wrapper around all other functions with the
#' \code{rda.conversion} prefix. It starts by calling
#' \code{\link{rda.conversion.ensureDataDir}} to ensure that the target
#' directory for the RDA files exists. Once that is assured the specified
#' \code{directory} will be checked via
#' \code{\link{rda.conversion.checkRawData}} for its existance. In case it
#' exists the list of contained files will be returned and be iteratively
#' loaded via \code{\link{rda.conversion.loadDataFile}} and saved as a RDA
#' via \code{\link{rda.conversion.saveAsRDA}}.
#'
#' @param directory The directory in which the RAW files should be searched.
#'
#' @seealso \code{\link{rda.conversion.ensureDataDir}}
#' @seealso \code{\link{rda.conversion.checkRawData}}
#' @seealso \code{\link{rda.conversion.loadDataFile}}
#' @seealso \code{\link{rda.conversion.saveAsRDA}}
rda.conversion.convertData <- function(directory = "inst/extdata") {
  rda.conversion.ensureDataDir()
  listOfFiles <- rda.conversion.checkRawData(directory)
  if (length(listOfFiles) > 0) {
    for (i in listOfFiles) {
      dat <- rda.conversion.loadDataFile(directory, i)
      rda.conversion.saveAsRDA(dat, i)
    }
  }
}

rda.conversion.convertData <- compiler::cmpfun(rda.conversion.convertData)
