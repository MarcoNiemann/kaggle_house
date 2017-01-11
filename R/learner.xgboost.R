#' Basic xgboost learner
#'
#' Parallel tuning function using xgboost that is either based on glmnet, boruta or the whole dataset.
#'
#' The default execution uses the whole training dataset. By setting either the \code{lasso} or
#' \code{boruta} parameter to true the number of features is reduced according to the results of
#' the feature selection. The number of rounds is set to 500 because including that parameter into
#' the set of tuning parameters leads to worse results. The xgboost learner is wrapper in a Filter
#' wrapper that uses chi squared as feature selection method. The result is three times cross validated
#' at maximum 1000 experiments using irace as a control structure.
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
#' KaggleHouse:::learner.xgboost(lasso=TRUE)
#'
learner.xgboost <- function(data = data_train_numeric_clean_imputed, lasso = FALSE, boruta = FALSE) {
  if (lasso && boruta) {
    echoln("Error: Only one feature selection is allowed")
    return(0)
  } else if (lasso) {
    feature.lasso()
    data = data[c(features_lasso, "SalePrice")]
  } else if (boruta) {
    feature.boruta()
    data = data[c(features_boruta, "SalePrice")]
  }
    parallelMap::parallelStartSocket(cpu_cores)

    kagglehouse_task = mlr::makeRegrTask(id = "kh", data = data, target = "SalePrice")
    basic_lrn = mlr::makeLearner("regr.xgboost", par.vals = list(nrounds = 500, nthread = 8))
    ps = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericParam("fw.perc", lower = 0.2, upper = 1),
      ParamHelpers::makeNumericParam("eta", lower = 0.001, upper = 0.999),
      ParamHelpers::makeDiscreteParam("max_depth",values = c(2:50)),
      ParamHelpers::makeNumericParam("min_child_weight", lower = 1, upper = 50),
      ParamHelpers::makeNumericParam("subsample", lower = 0, upper = 1),
      ParamHelpers::makeNumericParam("gamma", lower = 0, upper = 1000),
      ParamHelpers::makeNumericParam("alpha", lower = 0, upper = 1),
      ParamHelpers::makeNumericParam("lambda", lower = 0, upper = 1)
    )
    lrn = mlr::makeFilterWrapper(learner = basic_lrn, fw.method = "chi.squared")
    rdesc = mlr::makeResampleDesc("CV", iters = 3)
    ctrl =  mlr::makeTuneControlIrace(maxExperiments = 1000L)

    res = mlr::tuneParams(lrn, task = kagglehouse_task, resampling = rdesc, par.set = ps,
                          control = ctrl, show.info = TRUE)
    save(res, file = "learner.xgboost_result.RData")

    parallelMap::parallelStop()
}