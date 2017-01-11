#' Impute training data
#'
#' Imputation function for the training dataset using package \code{mice} and manual
#' substitution to remove missing values.
#'
#' Since some features are not missing at random imputation is not an appropriate approach.
#' Therefore the features \code{MasVnrArea}, \code{LotFrontage}, \code{Electrical} and
#' \code{GarafeYrBlt} are manually imputed. The remaining features that still contain
#' at least one missing value are solely imputed using \code{mice} with 50 maximum
#' iterations with seed 500 using predictive-mean matching. After the execution one
#' can access the variable \code{data_train_numeric_clean_imputed}.
#'
#' @param data Input data which is set by default to \code{data_train_numeric_clean}
#'
#'
imputation.train <- function(data = data_train_numeric_clean) {
  imputed_data <- data

  ## Impute MasVnrArea
  tmp <- mice::mice(data, m = 1, meth = 'pmm', maxit = 50, seed = 500)
  imputed_data$MasVnrArea[is.na(imputed_data$MasVnrArea)] <- unlist(tmp$imp$MasVnrArea)

  ## Impute LotFrontage
  imputed_data$LotFrontage[is.na(imputed_data$LotFrontage)] <- 0

  ## Impute Electrical
  imputed_data$Electrical[is.na(imputed_data$Electrical)] <- max(imputed_data$Electrical, na.rm = TRUE) + 1

  ## Impute GarageYrBlt
  imputed_data$GarageYrBlt[is.na(imputed_data$GarageYrBlt)] <- median(imputed_data$GarageYrBlt, na.rm = TRUE)

  assign(paste0(deparse(substitute(data)), "_imputed"), imputed_data, envir = .GlobalEnv)
}

#' Impute tst data
#'
#' Imputation function for the test dataset using package \code{mice} and manual
#' substitution to remove missing values.
#'
#' Since some features are not missing at random imputation is not an appropriate approach.
#' Therefore the features \code{MasVnrArea}, \code{LotFrontage}, \code{Electrical} and
#' \code{GarafeYrBlt} are manually imputed. The remaining features that still contain
#' at least one missing value are solely imputed using \code{mice} with 50 maximum
#' iterations with seed 500 using predictive-mean matching. After the execution one
#' can access the variable \code{data_train_numeric_clean_imputed}.
#'
#' @param data Input data which is set by default to \code{data_test_numeric_clean}
#'
#'
imputation.test <- function(data = data_test_numeric_clean) {
  imputed_data <- data

  ## Impute LotFrontage
  imputed_data$LotFrontage[is.na(imputed_data$LotFrontage)] <- 0

  ## Impute GarageYrBlt
  imputed_data$GarageYrBlt[is.na(imputed_data$GarageYrBlt)] <- median(imputed_data$GarageYrBlt, na.rm = TRUE)

  ## Impute remaining columns with mice (MSZoning, Exterior1st, Exterior2nd,  MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, BsmtFullBath, BsmtHalfBath, KitchenQual, Functional, GarageYrBlt, GarageCars, GarageArea, SaleType)
  tmp <- mice::mice(imputed_data, m = 1, meth = 'pmm', maxit = 50, seed = 500)
  imputed_data = complete(tmp)
  assign(paste0(deparse(substitute(data)), "_imputed"), imputed_data, envir = .GlobalEnv)
}
