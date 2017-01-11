#' Boruta Feature Selection - Wrapper
#'
#' Convenience method that calls \code{\link{feature.boruta.comp}} with preset
#' parameters that are commonly used for learners in this package.
#'
#' This method executes the packages Boruta wrapper
#' \code{\link{feature.boruta.comp}} with all parameters preset to fit the needs
#' of the package learners. Furthermore it stores each Boruta object after the
#' computation to the \code{output/feature_selection} directory. Before a new
#' computation is started the directory is checked for the existance of an
#' already computed Boruta object. If one is available and the \code{recompute}
#' flag is \code{FALSE} the previously computed object is loaded and used.
#' Finally the method binds all selected features (confirmed and tentative ones)
#' to the \code{.GlobalEnv} as the \code{features_boruta} variable.
#'
#' @param data      \code{data.frame} containing the data on which the Boruta
#'                  feature selection should be executed. Will be used to feed
#'                  the \code{target} and \code{predictors} variables of
#'                  \code{\link{feature.boruta.comp}}. Defaults to
#'                  \code{data_train_numeric_clean_imputed}.
#' @param recompute \code{boolean} switch that determines if the Boruta feature
#'                  selection should be repeated when the results of a prior
#'                  run have been found. Recommended when parameters have
#'                  changed. Defaults to \code{FALSE}.
#' @param desc      Additional comment that can be appended to the name of the
#'                  saved Boruta object. Can e.g. be used to store different
#'                  Boruta runs for different learners. Defaults to the empty
#'                  string.
#'
#' @examples
#'  KaggleHouse:::feature_boruta()
#'  KaggleHouse:::feature_boruta(recompute = T)
#'  KaggleHouse:::feature_boruta(recompute = T, desc = "_test_run_")
feature.boruta <- function(
  data = data_train_numeric_clean_imputed,
  recompute = F,
  desc = ""
) {
  file_name <- paste0("output/feature_selection/boruta", desc, ".RData")
  if (!file.exists(file_name) || recompute) {
    boruta <- feature.boruta.comp(
      target = data$SalePrice,
      predictors = data[-ncol(data)],
      fixNA = F,
      roughFix = F,
      variables = T,
      selected = T,
      formula = T,
      tentative = T,
      pValue = 0.01,
      mcAdj = T,
      maxRuns = 1000,
      doTrace = 2,
      holdHistory = T,
      getImp = Boruta::getImpRfZ,
      verbose = T
    )
    save(boruta, file = file_name)
  } else {
    load(file_name)
  }
  assign("features_boruta", boruta$Selected, envir = .GlobalEnv)
}

feature.boruta <- compiler::cmpfun(feature.boruta)

#' Boruta Feature Selection
#'
#' Wrapper around the \link{Boruta} package. Boruta is a so called all relevant
#' feature seletion wrapper, capable of working with each classifier outputting
#' variable importance measure (VIM). This function provides a wrapper ensuring
#' correct provision of input data and the potential to execute convenience
#' functions that e.g. provide regression formula output.
#'
#' The method first saves the name of the original \code{target} parameter so it
#' is potentially reusable for formula creation later on. In case the
#' \code{fixNA} switch is \code{TRUE}, all observations containing \code{NA}
#' values will be eliminated. If this should affect all observations an error
#' will be produced. Before executing the Boruta algorithm, the important input
#' parameters \code{target} and \code{predictors} will be checked via the
#' \code{\link{feature.boruta.fixNA}} method. Should any issues with the input
#' be found (wrong data types, differing lengths, \code{NA}s) an appropriate
#' error will be thrown.
#' Next the actual \code{\link{Boruta::Boruta}} algorithm is executed with the
#' provided parameters. Bortua than iteratively compares the importance of
#' shadow attributes with the original attributes. Those with a significantly
#' worse performance than shadow attributes will be rejected; those performing
#' significantly better will be confirmed.
#' Since the Boruta algorithm might not converge in the given \code{maxRuns}
#' iterations, the \code{\link{Boruta::TentativeRoughFix}} can be used to
#' resolve still missing values (given \code{roughFix} is \code{TRUE}).
#' Finally, depending on the values of the \code{variables} and \code{formula}
#' switches, a formula will be created and/or the confirmed/rejected/tentative
#' attributes are appended to the returned \code{Boruta} object.
#'
#' @param target      Response vector; factor for classification, numeric vector
#'                    for regression.
#' @param predictors  \code{data.frame} with predictors.
#' @param fixNA       \code{boolean} switch that decides how \code{NA} values
#'                    in the \code{predictors} and \code{target} variables will
#'                    be handled. \code{FALSE} would cause the \code{NA}s to be
#'                    ignored. \code{TRUE} will eliminate all observations
#'                    including a \code{NA} value. Default is \code{FALSE}.
#' @param roughFix    \code{boolean} switch that decides whether the
#'                    \code{\link{Boruta::TentativeRoughFix}} method will be
#'                    used to resolve potentially remaining undecided variables.
#'                    Default is \code{FALSE}.
#' @param variables   \code{boolean} switch that decides whether the variables
#'                    of all three categories (Confirmed, Tentative, Rejected)
#'                    will be appended to the returned \code{Boruta} object.
#'                    Default is \code{FALSE}.
#' @param selected    \code{boolean} switch that decides whether the confirmed
#'                    and tentative varialbes will be added as a combined vector
#'                    to the \code{Boruta} object. Default is \code{FALSE}.
#'                    Only works when \code{variables} is \code{TRUE}.
#' @param formula     \code{boolean} switch that decides whether a formula will
#'                    be appended to the returned \code{Boruta} object. This
#'                    formula will relate the \code{target} with all confirmed
#'                    \code{predictors}. Depending on the \code{tentative}
#'                    switch the tentative variables might be added as well.
#'                    Defaults to \code{FALSE}.
#' @param tentative   \code{boolean} switch that decides whether tentative
#'                    attributes will be considered for a formula. Default is
#'                    \code{FALSE}.
#' @param pValue      Confidence level. Default value should be used. Default is
#'                    0.01.
#' @param mcAdj       If set to \code{TRUE}, a multiple comparisons adjustment
#'                    using the Bonferroni method will be applied. Default value
#'                    should be used; older (1.x and 2.x) versions of Boruta
#'                    were effectively using FALSE. Default value is
#'                    \code{TRUE}.
#' @param maxRuns     Maximal number of importance source runs. You may increase
#'                    it to resolve attributes left tentative. Default is 100.
#' @param doTrace     Verbosity level. 0 means no tracing, 1 means reporting
#'                    decision about each attribute as soon as it is justified,
#'                    2 means same as 1, plus reporting each importance source
#'                    run. Default is 0.
#' @param holdHistory If set to \code{TRUE}, the full history of importance is
#'                    stored and returned as the \code{ImpHistory} element of
#'                    the result. Can be used to decrease a memory footprint of
#'                    Boruta in case this side data is not used, especially when
#'                    the number of attributes is huge; yet it disables plotting
#'                    of such made Boruta objects and the use of the
#'                    \code{\link{Boruta::TentativeRoughFix}} function. Default
#'                    is \code{FALSE}.
#' @param getImp      Function used to obtain attribute importance. The default
#'                    is \code{\link{getImpRfZ}}, which runs random forest from
#'                    the \code{\link{ranger}} package and gathers Z-scores of
#'                    mean decrease accuracy measure. It should return a numeric
#'                    vector of a size identical to the number of columns of its
#'                    first argument, containing importance measure of
#'                    respective attributes. Any order-preserving transformation
#'                    of this measure will yield the same result. It is assumed
#'                    that more important attributes get higher importance.
#'                    \code{+-Inf} are accepted, \code{NaNs} and \code{NAs} are
#'                    treated as 0s, with a warning. Default is
#'                    \code{\link{Boruta::getImpRfZ}}.
#' @param verbose     \code{boolean} switch that decides whether the error
#'                    output will provide more verbose information. Default is
#'                    \code{FALSE}.
#'
#' @return \code{Boruta} object as it is also returned by the underlying
#'         \code{\link{Boruta::Boruta}} method. This default return value can
#'         include severeal extensions, depending on parameters like
#'         \code{formula}:
#'            \item{target}{The name of the target vector.}
#'            \item{variables}{Variable names of all three categories
#'                             (Confirmed, Tentative, Rejected) }
#'            \item{selected}{Variables names of confirmed and tentative
#'                            variables in one vector.}
#'            \item{formula}{Formula of the form
#'                            \code{target ~ predictors.(confirmed/tentative)}}
#'
#' @seealso \code{\link{Boruta::Boruta}}
#' @seealso \code{\link{feature.boruta.fixNA}}
#' @seealso \code{\link{feature.boruta.tentative}}
#' @seealso \code{\link{feature.boruta.variables}}
#' @seealso \code{\link{feature.boruta.selected}}
#' @seealso \code{\link{feature.boruta.formula}}
#' @seealso \code{\link{feature.boruta.checkInputParams}}
#'
#' @examples
#'  KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = T, verbose = T
#'  )
#'
#' @references
#'  Miron B. Kursa, Witold R. Rudnicki (2010).
#'  Feature Selection with the Boruta Package.
#'  \emph{Journal of Statistical Software, 36(11)}, p. 1-13.
#'  URL: \url{http://www.jstatsoft.org/v36/i11/}
#'
feature.boruta.comp <- function(
  target, predictors, fixNA = F, roughFix = F, variables = F, selected = F,
  formula = F, tentative = F, pValue = 0.01, mcAdj = T, maxRuns = 100,
  doTrace = 0, holdHistory = T, getImp = Boruta::getImpRfZ, verbose = F, ...
) {

  # Save the target variable name.
  targetVar <- deparse(substitute(target))

  # If fixNA flag is set, NA values are removed.
  if (fixNA) {
    fixSet <- feature.boruta.fixNA(target, predictors)
    target <- fixSet$target$target
    predictors <- fixSet$predictors
  }

  feature.boruta.checkInputParams(
    target, predictors, checkNA = T, verbose = verbose
  )

  boruta <- Boruta::Boruta(
    x = predictors, y = target, pValue = pValue, mcAdj = mcAdj,
    maxRuns = maxRuns, doTrace = doTrace, holdHistory = holdHistory,
    getImp = getImp, ...
  )

  # If roughFix flag is set and tentative attributes remain they are resolved.
  if (roughFix && feature.boruta.tentative(boruta)) {
    boruta <- Boruta::TentativeRoughFix(boruta)
  }

  # Ensure that only the target variable name is used (excl. any potentially
  # prefixed dataset).
  boruta$target <- cpp_regex_selector_name(targetVar)

  if (variables) {
    var.split <- feature.boruta.variables(boruta)
    boruta$Confirmed <- var.split$Confirmed
    boruta$Tentative <- var.split$Tentative
    boruta$Rejected <- var.split$Rejected
  }

  if (variables && selected) {
    boruta$Selected <- feature.boruta.selected(boruta)
  } else if (variables && !selected) {
    warning("Could not add $selected. variables has to be TRUE.")
  }

  if (formula) {
    boruta$formula <- feature.boruta.formula(boruta, tentative)
  }

  return(boruta)
}

feature.boruta.comp <- compiler::cmpfun(feature.boruta.comp)


#' Remove \code{NA} Containing Observations
#'
#' Filter all observations from the \code{target} and \code{predictors} variable
#' that contain \code{NA} values to obtain a \code{NA} dataset usable by the
#' Boruta classifiers.
#'
#' The method first carries out a reduced parameter check via
#' \code{\link{feature.boruta.checkInputParams}}. Afterwards both \code{target}
#' and \code{predictors} are combined to a unified \code{data.frame}. This
#' \code{data.frame} is then cleaned from \code{NA}s by the
#' \code{\link{na.omit}} function. Before splitting the \code{data.frame} into
#' a target and a predictor variable again, it is checked that at least one
#' observation prevailed. If that is not the case, the method is aborted with
#' an error.
#'
#' @param target      Response vector; factor for classification, numeric vector
#'                    for regression.
#' @param predictors  \code{data.frame} with predictors.
#'
#' @return The \code{NA}-cleaned \code{data.frame} will be returned with two
#'         additional parameters:
#'         \item{target}{Containing the \code{NA}-cleaned target variable.}
#'         \item{predictors}{Containing the \code{NA}-cleaned predictors.}
#'
#' @examples
#'  KaggleHouse:::feature.boruta.fixNA(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81]
#' )
#'
feature.boruta.fixNA <- function(target, predictors) {
  # Do reduced parameter check. Skip the NA check.
  feature.boruta.checkInputParams(target, predictors, checkNA = F, verbose = F)

  fixSet <- cbind(predictors, target)
  fixSet <- na.omit(fixSet)

  if (NROW(fixSet) == 0) {
    stop("All observations contain NA values. Cleaning not possible!")
  }

  fixSet$target <- fixSet[NCOL(fixSet)]
  fixSet$predictors <- fixSet[-NCOL(fixSet)]

  return(fixSet)
}

feature.boruta.fixNA <- compiler::cmpfun(feature.boruta.fixNA)


#' Check For Tentative Variables
#'
#' The method checks a \code{Boruta} object for the decision status by checking
#' if some variables are still tentative.
#'
#' This method evaluates the \code{Boruta} object. In case some other object is
#' provided to the function it will abort with an error. All correctly assigned
#' objects will then be checked for variables that still have the tentative
#' status. If at least one is still tentative a \code{TRUE} will be returned.
#'
#' @param boruta  \code{Boruta} object obtained by the execution of
#'                \code{\link{feature.boruta}} method.
#'
#' @return \code{boolean} value that is \code{TRUE} as soon as one tentative
#'         variable is found. \code{FALSE} otherwise.
#'
#' @examples
#'  boruta <- KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = T, verbose = T
#'  )
#'
#'  KaggleHouse:::feature.boruta.tentative(boruta)
#'
feature.boruta.tentative <- function(boruta) {
  if (class(boruta) != "Boruta") {
    stop("boruta is no Boruta object!")
  }

  return(any(boruta$finalDecision == "Tentative"))
}

feature.boruta.tentative <- compiler::cmpfun(feature.boruta.tentative)


#' Obtain List of Confirmed, Tentative and Rejected Variables
#'
#' The method obtains a list of three vectors containing confirmed, tentative
#' and rejected variables.
#'
#' This method evaluates the \code{Boruta} object. In case some other object is
#' provided to the function it will abort with an error. From correctly provided
#' \code{Boruta} objects a list of all evaluated variable names will be
#' obtained. Based on the \code{finalDecision} variable of the \code{Boruta}
#' object the variable names will be split into three vectors representing
#' confirmed, tentative and rejected variables. These vectors are combined in a
#' list and are returned as such.
#'
#' @param boruta  \code{Boruta} object obtained by the execution of
#'                \code{\link{feature.boruta.comp}} method.
#'
#' @return A list with the three entries:
#'          \item{Confirmed}{Vector of confirmed variables.}
#'          \item{Tentative}{Vector of tentative variables.}
#'          \item{Rejected}{Vector of rejected variables.}
#'
#' @examples
#'  boruta <- KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = T, verbose = T
#'  )
#'
#'  KaggleHouse:::feature.boruta.variables(boruta)
#'
feature.boruta.variables <- function(boruta) {
  if (class(boruta) != "Boruta") {
    stop("boruta is no Boruta object!")
  }

  var.names <- names(boruta$finalDecision)
  var.split <- split(var.names, boruta$finalDecision)

  return(var.split)
}

feature.boruta.variables <- compiler::cmpfun(feature.boruta.variables)


#' Create Formula Based on Boruta Selected Features
#'
#' The method creates a formula that puts a target value in relationship to
#' features selected by the Boruta algorithm.
#'
#' This method evaluates the \code{Boruta} object. In case some other object is
#' provided to the function it will abort with an error. Given correct inputs
#' the function accesses the \code{target} property of the custom \code{Boruta}
#' object returned by \code{\link{feature.boruta}} and also obtains the
#' categorized list of the evaluated variables via
#' \code{\link{feature.boruta.variables}}. With these inputs a formula of the
#' structure \code{target ~ predictorVar1 + ...} is created which can then e.g.
#' be reused for regression.
#'
#' @param boruta    \code{Boruta} object obtained by the execution of
#'                  \code{\link{feature.boruta}} method.
#' @param tentative \code{boolean} switch to decide whether tentative variables
#'                  should be part of the created formula. Default is
#'                  \code{FALSE}.
#'
#' @return \code{formula} object that represents the relationship between the
#'         predictors and the target variable which has been evaluated by the
#'         Boruta algorithm. The form will be \code{target ~ predictorVars}
#'         where \code{predictorVars} will only be confirmed (and tentative)
#'         variables.
#'
#' @examples
#'  boruta <- KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = T, verbose = T
#'  )
#'
#'  KaggleHouse:::feature.boruta.formula(boruta, tentative = T)
#'
feature.boruta.formula <- function(boruta, tentative = F) {
  if (class(boruta) != "Boruta") {
    stop("boruta is no Boruta object!")
  }

  boruta.variables <- feature.boruta.variables(boruta)
  predictors <- paste(boruta.variables$Confirmed, collapse = " + ")

  if (tentative && length(boruta.variables$Tentative) > 0) {
    predictors <- paste(predictors,
                        paste(boruta.variables$Tentative, collapse = " + "),
                        sep = " + "
    )
  }

  return(as.formula(paste(boruta$target, " ~ ", predictors)))
}

feature.boruta.formula <- compiler::cmpfun(feature.boruta.formula)


#' Check Boruta Feature Selection Input Parameters
#'
#' The method ensures that the parameters of the \code{\link{feature.boruta}}
#' function are valid.
#'
#' This method analyses the \code{target} and \code{predictors} input variables
#' of the \code{\link{feature.boruta}} function. It conducts two mandatory
#' checks and one optional check. The first of the two mandatory checks
#' evaluates whether the correct data types are used. So the \code{target}
#' variable should be a \code{vector} while the \code{predictors} variable is
#' supposed to be a \code{data.frame}. As a second check it is ensured that
#' both variables have the same number of rows, which implies that the number of
#' considered observation is equal for both. If one of the checks fail,
#' execution will be aborted with an error explaining the reasons.
#' The optional check evaluates the presence of \code{NA} values as these are
#' not useable for many of the Boruta classifiers. In case either \code{target}
#' and/or \code{predictors} contain only one \code{NA} a warning is issued. If
#' the \code{verbose} switch is \code{TRUE}, the error will provide a list of
#' \code{predictors} columns which contain \code{NA} values.
#'
#' @param target      Response vector; factor for classification, numeric vector
#'                    for regression.
#' @param predictors  \code{data.frame} with predictors.
#' @param checkNA     \code{boolean} switch that decides whether the method will
#'                    conduct a check for \code{NA} values or not. Default is
#'                    \code{FALSE}.
#' @param verbose     \code{boolean} switch that decides whether the error
#'                    output will provide more verbose information. Default is
#'                    \code{FALSE}.
#'
#' @examples
#'  KaggleHouse:::feature.boruta.checkInputParams(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'    checkNA = T, verbose = T
#'  )
#'
feature.boruta.checkInputParams <- function(
  target, predictors, checkNA = F, verbose = F
) {
  target.vec <- is.vector(target)
  target.rows <- NROW(target)
  target.isNa <- any(is.na(target))
  predictors.df <- is.data.frame(predictors)
  predictors.rows <- NROW(predictors)
  predictors.isNa <- any(is.na(predictors))

  if (!target.vec || !predictors.df) {
    stop(paste("Target and/or predictor have wrong data type.", "\n",
               "Target Vector:", target.vec, "\n",
               "Predictor Dataframe:", predictors.df, "\n")
    )
  }

  if (target.rows != predictors.rows) {
    stop(paste("Target and Predictors differ in terms of observations.", "\n",
               "Target:", target.rows, "\n",
               "Observations:", predictors.rows, "\n")
    )
  }

  if ((target.isNa || predictors.isNa) && checkNA == T) {
    if (verbose && predictors.isNa) {
      stop(paste("Data contains NA values.", "\n",
                 "Target NAs:", target.isNa, "\n",
                 "Predictor NAs:", predictors.isNa, "\n",
                 "NA Columns:", paste(colnames(
                   is.na(predictors)), collapse = " + "), "\n"
      )
      )
    }
    if (!verbose) {
      stop(paste("Data contains NA values.", "\n",
                 "Target NAs:", target.isNa, "\n",
                 "Predictors NAs", predictors.isNa, "\n"))
    }
  }
}

feature.boruta.checkInputParams <-
  compiler::cmpfun(feature.boruta.checkInputParams)

#' Create PDF and Text Reports About Selected Boruta Features
#'
#' This methods provides a PDF with the major info graphics related to Boruta
#' feature selection, as well as a text file with all confirmed, tentative and
#' rejected variables including the formula generated from them.
#'
#' In a first step this method will generate two plots for the given
#' \code{Boruta} object: One plot that will show boxplots for all features,
#' as well as their importance and their selection status (e.g. a red feature
#' has been discared, a green one confirmed). Additionally the imputation
#' history is added, which shows the importance (and acceptance status) of each
#' feature over the time. These two graphics are combined into one PDF file.
#' Since it is versioned with time information, each call to this method will
#' automatically generate a new report. The time stamp thus allows to version
#' different \code{Boruta} objects.
#' In case the \code{Boruta} object has been generated with
#' \code{\link{feature.boruta.comp}} and contains the \code{Confirmed} (etc.)
#' variables an additional text file is generated and filled with all the
#' information regarding those features. If the \code{formula} switch has
#' been activated the generated formula will also be added to that text file.
#' Similar to the PDF file the txt file is versioned via timestamps as part
#' of the file name.
#'
#' @param boruta  The \code{Boruta} object for which a report should be
#'                generated.
#'
#' @examples
#'  boruta <- KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = F, variables = T, selected = T, formula = T,
#'     verbose = T
#'  )
#'  KaggleHouse:::feature.boruta.report(boruta)
#'
feature.boruta.report <- function(boruta) {
  if (class(boruta) != "Boruta") {
    stop("boruta is no Boruta object!")
  }

  out.dir <- paste0(dir_presentation, "/feature_selection")
  file.name.pdf <- paste0("boruta_report_",
                          format(Sys.time(), "%Y-%M-%d_%H-%M-%S"),
                          ".pdf")
  file.name.txt <- paste0("boruta_report_",
                          format(Sys.time(), "%Y-%M-%d_%H-%M-%S"),
                          ".txt")

  util.ensure.dir(out.dir)

  pdf(file = paste(out.dir, file.name.pdf, sep = "/"), width = 20, height = 10)
  par(mar = c(10.1, 4.1, 4.1, 1.1))
  Boruta:::plot.Boruta(boruta, las = 2, xlab = "", main = "Boruta Result Plot")
  legend('topleft',
         c("Confirmed", "Tentative", "Rejected", "Shadow"),
         col = c("green", "yellow", "red", "blue"),
         lwd = 10, xjust = 0.5, yjust = 0.5)
  mtext(text = "Attributes", side = 1, line = 8)

  par(mar = c(5.1, 4.1, 4.1, 2.1))
  Boruta::plotImpHistory(boruta)
  dev.off()

  sink(paste(out.dir, file.name.txt, sep = "/"))
  # Checking for one category is sufficient since always all or none is
  # present in the Boruta object.
  if ("Confirmed" %in% attributes(boruta)$names) {
    echoln(paste(">", "Confirmed Variables"))
    print(boruta$Confirmed)

    echoln("\n")
    echoln(paste(">", "Tentative Variables"))
    print(boruta$Tentative)

    echoln("\n")
    echoln(paste(">", "Rejected Variables"))
    print(boruta$Rejected)
  }

  if ("formula" %in% attributes(boruta)$names) {
    echoln("\n")
    echoln(paste(">", "Formula"))
    print(boruta$formula)
  }
  sink()
}

feature.boruta.report <- compiler::cmpfun(feature.boruta.report)

#' Provide all Non-Rejected Boruta Features
#'
#' This method combines the features that have either been confirmed or have
#' been marked as tentative in the Boruta algorithm into one variable.
#'
#' This method usually is not meant to be applied by a package user. In case it
#' is still used to obtain the confirmed and tentative variables of a
#' \code{Boruta} object it has to be ensured that it has the variables
#' \code{Confirmed} and \code{Tentative}. These can be obtained via the
#' \code{variables} switch of the \code{\link{feature.boruta.comp}} method that
#' has to be \code{TRUE}.
#' Then the features contained in both variables will be combined into a
#' \code{Selected} variable. Finally the method binds the \code{Selected}
#' variable to the \code{.GlobalEnv} as \code{features_boruta}.
#'
#' @param boruta  The \code{Boruta} object to which the \code{Selected} variable
#'                should be attached.
#'
#' @return \code{Boruta} object with an attached \code{Selected} variable
#'         containing the confirmed and tentative features.
#'
#' @examples
#'  boruta <- KaggleHouse:::feature.boruta(
#'    target = data_train_na$SalePrice, predictors = data_train_na[-81],
#'     fixNA = T, roughFix = F, variables = T, verbose = T
#'  )
#'  KaggleHouse:::feature.boruta.selected(boruta)
#'
feature.boruta.selected <- function(boruta) {
  selected <- c(boruta$Confirmed, boruta$Tentative)
  assign("features_boruta", selected, envir = .GlobalEnv)
  return(selected)
}

feature.boruta.selected <- compiler::cmpfun(feature.boruta.selected)
