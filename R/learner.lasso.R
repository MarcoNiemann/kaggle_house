#' Basic glmnet learner
#'
#' Parallel tuning function using glmnet that is either based on glmnet, boruta or the whole dataset.
#'
#' The default execution uses the whole training dataset. By setting either the \code{lasso} or
#' \code{boruta} parameter to true the number of features is reduced according to the results of
#' the feature selection. The glmnet learner is wrapper in a Filter wrapper that uses chi squared
#' as feature selection method. The result is three times cross validated
#' at maximum 2000 experiments using irace as a control structure.
#'
#' @param data Input data which is default set to the numeric, imputed and cleaned training dataset
#'
#' @param lasso Boolean flag that shrinks data to the features of \code{feature.lasso()} stored in
#' the variable \code{features_lasso}
#'
#' @param boruta Boolean flag that shrinks data to the features of \code{boruta.lasso()} stored in
#' the variable \code{features_boruta}
#'
#' @return 0 as error output if both flags are set to true
#'
#' @examples
#' KaggleHouse:::learner.lasso(lasso=TRUE)
#'
learner.lasso <- function(data = data_train_numeric_clean_imputed, lasso = FALSE, boruta = FALSE) {
  if (lasso && boruta) {
    echoln("Error: Only one feature selection is allowed.")
    return(0)
  } else if (lasso) {
    echoln("The learner is using only features provided by lasso feature selection.")
    feature.lasso()
    data = data[c(features_lasso, "SalePrice")]
  } else if (boruta) {
    echoln("The learner is using only features provided by boruta feature selection.")
    feature.boruta()
    data = data[c(features_boruta, "SalePrice")]
  }

  parallelMap::parallelStartSocket(cpu_cores)

  kagglehouse_task = mlr::makeRegrTask(id = "kh", data = data, target = "SalePrice")
  basic_lrn = mlr::makeLearner("regr.glmnet")
  ps = ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam("fw.perc", lower = 0.2, upper = 1),
    ParamHelpers::makeNumericParam("s", lower = 0, upper = 1000),
    ParamHelpers::makeNumericParam("alpha", lower = 0, upper = 1),
    ParamHelpers::makeNumericParam("lambda.min.ratio", lower = 0, upper = 1),
    ParamHelpers::makeNumericParam("fdev", lower = 0, upper = 1),   #--
    ParamHelpers::makeNumericParam("eps", lower = 0, upper = 1),   #--
    ParamHelpers::makeNumericParam("pmin", lower = 0, upper = 1),   #--
    ParamHelpers::makeNumericParam("exmx", lower = -500, upper = 500),   #--
    ParamHelpers::makeNumericParam("prec", lower = -500, upper = 500),   #--
    ParamHelpers::makeIntegerParam("nlambda", lower = 100, upper = 1000),
    ParamHelpers::makeIntegerParam("pmax", lower = 0, upper = 1000),
    ParamHelpers::makeIntegerParam("maxit", lower = 100000, upper = 150000),
    ParamHelpers::makeIntegerParam("mnlam", lower = 5, upper = 1000),   #--
    ParamHelpers::makeIntegerParam("mxit", lower = 1, upper = 1000) #---
  )
  lrn = mlr::makeFilterWrapper(learner = basic_lrn, fw.method = "chi.squared")
  rdesc = mlr::makeResampleDesc("CV", iters = 3)
  ctrl =  mlr::makeTuneControlIrace(maxExperiments = 2000L)
  res = mlr::tuneParams(lrn, task = kagglehouse_task, resampling = rdesc, par.set = ps,
                   control = ctrl, show.info = TRUE)
  save(res, file = "learner.lasso_result.RData")

  parallelMap::parallelStop()
}
