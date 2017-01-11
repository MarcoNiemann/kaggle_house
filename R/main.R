#' Function for generating datasets
#'
#' In general the first function that should be executed when the package is loaded.
#' Generates the two dataset \code{data_train_numeric_clean_imputed} and \code{data_test_numeric_clean_imputed}.
#'
#' First the function generates data views for e.g. distinguishing categorical and ordinal variables. Next the
#' data is cleaned by removing unnecessary columns and rows. After that imputation takes place by manually
#' and automatically(mice with pmm) substituting missing values.
#'
#'@export
run_generate_data <- function() {

  data.generate_data_views()

  prepare.transform_data(data_train, c("Alley",
                                  "MasVnrType",
                                  "BsmtQual",
                                  "BsmtCond",
                                  "BsmtExposure",
                                  "BsmtFinType1",
                                  "BsmtFinType2",
                                  "FireplaceQu",
                                  "GarageType",
                                  "GarageFinish",
                                  "GarageQual",
                                  "GarageCond",
                                  "PoolQC",
                                  "Fence",
                                  "MiscFeature"))
  prepare.transform_data(data_test, c("Alley",
                                 "MasVnrType",
                                 "BsmtQual",
                                 "BsmtCond",
                                 "BsmtExposure",
                                 "BsmtFinType1",
                                 "BsmtFinType2",
                                 "FireplaceQu",
                                 "GarageType",
                                 "GarageFinish",
                                 "GarageQual",
                                 "GarageCond",
                                 "PoolQC",
                                 "Fence",
                                 "MiscFeature"))

  ## Remove four rows(outlier) and two columns(Id and Utilities)
  preprocess.generate_cleaned_data(data_train_numeric, c(524, 692, 1183, 1299), c(1, 10))
  preprocess.generate_cleaned_data(data_test_numeric, c(), c(1, 10))

  ## Imputation for the test and train dataset
  imputation.train()
  imputation.test()

  ## Change the datatype of each column from integer to numeric(necessary for xgboost)
  data_train_numeric_clean_imputed <<- data.frame(lapply(data_train_numeric_clean_imputed, function(col) {as.numeric(col)}))
  data_test_numeric_clean_imputed <<- data.frame(lapply(data_test_numeric_clean_imputed, function(col) {as.numeric(col)}))
}

#' Descriptive analysis
#'
#' Mainly used to output plots that are used to get a general understanding of the given data.
#'
#' The plots and text documents are saved under the /output directory. Besides generating a
#' textual summary of the whole training dataset this function also generates histograms, barplots,
#' scatterplots and qq-plots.
#'
#' @param train          Training dataset
#'
#' @param train_no_na    Training dataset only containing numerical values and no missing data
#'
#'@export
run_descriptive <- function(train = data_train, train_no_na = data_train_numeric_clean_imputed) {

  ## General plotting functions
  general_summary_df(train)

  general_hist(train, "1")

  general_barplot()

  general_plot(train_no_na, "1")

  general_qq(train, "1")

  ## Plot all features against Saleprice
  plot_against_var(
    train_no_na[, 1:ncol(train_no_na)-1],
    data.frame(SalePrice = train_no_na[, ncol(train_no_na)])
  )
}

#' Main stacked learner
#'
#' Executes the stacked learner on the train and test dataset
#'
#' Wrapper function that is exported to the user of the package.
#'
#' @param train  Cleaned, imputed and numeric training dataset
#' @param test   Cleaned, imputed and numeric test dataset
#'
#'@export
run_learner <- function(train = data_train_numeric_clean_imputed, test = data_test_numeric_clean) {
  learner.stacked(train, test)
}

