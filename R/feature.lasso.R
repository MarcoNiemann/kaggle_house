#' Lasso feature selection
#'
#' When executed an optimal set of features according to the lambda value
#' that minimizes the loss is bound to a global variable \code{features_lasso}.
#'
#' Default dataset is \code{data_train_numeric_clean_imputed}. The family
#' is \code{gaussian} and for measuring the goodness \code{auc} is chosen.
#'
#' @param data    Input data
#'
#' @examples      feature.lasso(data=BostonHousing)
#'
feature.lasso <- function(data = data_train_numeric_clean_imputed) {
  set.seed(999)
  cv.lasso <- glmnet::cv.glmnet(data.matrix(data[,1:ncol(data)-1]),
                                data[,ncol(data)],
                                family='gaussian',
                                alpha=1,
                                standardize=TRUE,
                                parallel=TRUE,
                                type.measure='auc')
  all_features = as.matrix(coef(cv.lasso, cv.lasso$lambda.min))
  features = attr(all_features, "dimnames")[[1]][all_features != 0]
  features = features[-1]
  assign("features_lasso", features, envir = .GlobalEnv)
}

# Results
##plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)
