#' Submit Results to Kaggle
#'
#' Upload a preformatted csv file with the predicted test results to Kaggle.
#'
#' This method allows to upload the predicted output in csv format to the
#' Kaggle competition. It bases on the Kaggle CLI tool by floydwch which is a
#' Python CLI tool. This requires an existing Python installation as well as
#' having the Kaggle CLI tool installed (with Python and pip via
#' \code{pip install kaggle-cli}). With the requirements given this method
#' works as an R wrapper around this tool.
#'
#' @param filepath    Path to the \code{csv} file with the predicted data to be
#'                    uploaded.
#' @param username    Kaggle username to login to the Kaggle plattform.
#' @param password    Kaggle password to login to the Kaggle plattform.
#' @param competition Name of the Kaggle competition the submission is intended
#'                    for.
#' @param message     Additional message that describes the submission. Is
#'                    optional and by default empty.
#'
#' @seealso Makes use of the Kaggle CLI tool by floydwch which can be found
#'          at \url{https://github.com/floydwch/kaggle-cli}.
#'
#' @examples
#'  KaggleHouse:::api.submit(
#'    filepath = "sample_submission.csv",
#'    username = "kaggle_user",
#'    password = "abc123",
#'    competition = "house-prices-advanced-regression-techniques"
#'  )
api.submit <- function(
  filepath,
  username,
  password,
  competition,
  message = ""
) {
  cmd <- paste("kg submit", filepath,
               "-u", username,
               "-p", password,
               "-c", competition)
  if (message != "") { cmd <- paste(cmd, "-m", message) }

  sys_name <- Sys.info()['sysname']

  if (sys_name == "Windows") {
    system(cmd)
  } else if (sys_name == "Linux") {
    cmd <- paste("bin/bash -c", paste0("'", cmd, "'"))
    system(cmd)
  }
}