#' Stacked learner
#'
#' Combines the parameter sets from the tuning of the basic learners to predict the final Saleprice
#' using linear regression.
#'
#' Uses the \code{features_boruta} to select only features that are considered important by \code{feature.boruta()}.
#' This stacked learner uses xgboost, lasso with the tuned parameters and deeplearning with 10 hidden layers each
#' containing 300 nodes as basic learners. At last linear regression is used to predict the Saleprice using
#' all three predictions. The result is stored in \code{final_submission_stacked_learner.csv} and can be directly
#' uploaded to kaggle.
#'
#' @param input_train Input data which is by default set to \code{data_train_numeric_clean_imputed}
#'
#' @param input_test Input data which is by default set to \code{data_test_numeric_clean_imputed}
#'
#' @examples
#'  KaggleHouse:::learner.stacked()
#'
learner.stacked <- function(input_train = data_train_numeric_clean_imputed, input_test = data_test_numeric_clean_imputed) {

  ## Only consider features given by boruta feature selection
  feature.boruta()
  input_train = input_train[c(features_boruta, "SalePrice")]
  input_test = input_test[features_boruta]

  kagglehouse_task = mlr::makeRegrTask(id = "kh", data = input_train, target = "SalePrice")

  ## Train and predict with the tuned xgboost learner
  base_lrn_xgboost = mlr::makeLearner("regr.xgboost", par.vals = list(nrounds = 1500,
                                                              eta=0.0202,
                                                              max_depth=23,
                                                              min_child_weight=1.8,
                                                              subsample=0.702,
                                                              gamma=22.3,
                                                              alpha=0.0371,
                                                              lambda=0.428))
  lrn_xgboost = mlr::makeFilterWrapper(learner = base_lrn_xgboost, fw.method = "chi.squared", fw.perc = 0.389)
  xgboost_model = mlr::train(lrn_xgboost, kagglehouse_task)
  predictions_xgboost_train = predict(xgboost_model, newdata = input_train[-ncol(input_train)])$data$response
  predictions_xgboost_test = predict(xgboost_model, newdata = input_test)$data$response

  ## Train and predict with the tuned elastic-net learner
  base_lrn_lasso = mlr::makeLearner("regr.glmnet", par.vals = list(s=356, alpha=0.859, lambda.min.ratio=0.0165, fdev=0.515, eps=0.017, pmin=0.696, exmx=-307, prec=-479, nlambda=383, pmax=329, maxit=116368, mnlam=829, mxit=130))
  lrn_lasso = mlr::makeFilterWrapper(learner = base_lrn_lasso, fw.method = "chi.squared", fw.perc = 0.67)
  lasso_model = mlr::train(lrn_lasso, kagglehouse_task)
  predictions_lasso_train = predict(lasso_model, newdata = input_train[-ncol(input_train)])$data$response
  predictions_lasso_test = predict(lasso_model, newdata = input_test)$data$response

  ## Train and predict with deeplearning
  base_lrn_deeplearning = mlr::makeLearner("regr.h2o.deeplearning", par.vals = list(hidden=rep(300,10), epochs = 3000))
  #h2o.shutdown()
  h2o_init = h2o::h2o.init(nthreads = -1)
  deeplearning_model = mlr::train(base_lrn_deeplearning, kagglehouse_task)
  h2o::h2o.saveModel(deeplearning_model$learner.model, path = "./")
  predictions_deeplearning_train = predict(deeplearning_model, newdata = input_train[-ncol(input_train)])$data$response
  predictions_deeplearning_test = predict(deeplearning_model, newdata = input_test)$data$response

  ## Use linear regression as the stacked learner
  predictions_train = data.frame(pred1 = predictions_xgboost_train,
                                 pred2 = predictions_lasso_train,
                                 pred3 = predictions_deeplearning_train)
  predictions_test = data.frame(pred1 = predictions_xgboost_test,
                                pred2 = predictions_lasso_test,
                                pred3 = predictions_deeplearning_test)
  lin_model = lm(input_train$SalePrice ~ predictions_train$pred1 + predictions_train$pred2 + predictions_train$pred3)

  result_train = as.numeric(predict.lm(lin_model))
  result_test_lm = lin_model$coefficients[1] +
    lin_model$coefficients[2] * predictions_test$pred1 +
    lin_model$coefficients[3] * predictions_test$pred2 +
    lin_model$coefficients[4] * predictions_test$pred3

  ## Write into .csv file for submission
  csv_output = data.frame(Id = 1461:2919, SalePrice = result_test_lm)
  util.generate.submit(csv_output, "final_submission_stacked_learner.csv")
}
