################################################################################
#                             DATA VIEWS                                       #
################################################################################

#' Data view generation function
#'
#' This function creates five different variables containing the names of features.
#'
#' Calling this function is optional. It is useful when analysing different types
#' of features as e.g. continuous, discrete, nominal, ordinal and the target feature
#' Saleprice. All variables are bound the base environment.
#'
data.generate_data_views <- function() {
  e <- baseenv()

  e$iowa.houses.continuous.vars <- c(
    "LotFrontage",
    "LotArea",
    "MasVnrArea",
    "BsmtFinSF1",
    "BsmtFinSF2",
    "BsmtUnfSF",
    "TotalBsmtSF",
    "X1stFlrSF",
    "X2ndFlrSF",
    "LowQualFinSF",
    "GrLivArea",
    "GarageArea",
    "WoodDeckSF",
    "OpenPorchSF",
    "EnclosedPorch",
    "X3SsnPorch",
    "ScreenPorch",
    "PoolArea",
    "MiscVal"
  )

  e$iowa.houses.discrete.vars <- c(
    "YearBuilt",
    "YearRemodAdd",
    "BsmtFullBath",
    "BsmtHalfBath",
    "FullBath",
    "HalfBath",
    "BedroomAbvGr",
    "KitchenAbvGr",
    "TotRmsAbvGrd",
    "Fireplaces",
    "GarageYrBlt",
    "GarageCars",
    "MoSold",
    "YrSold"
  )

  e$iowa.houses.nominal.vars <- c(
    "MSSubClass",
    "MSZoning",
    "Street",
    "Alley",
    "LandContour",
    "LotConfig",
    "Neighborhood",
    "Condition1",
    "Condition2",
    "BldgType",
    "HouseStyle",
    "RoofStyle",
    "RoofMatl",
    "Exterior1st",
    "Exterior2nd",
    "MasVnrType",
    "Foundation",
    "Heating",
    "CentralAir",
    "GarageType",
    "MiscFeature",
    "SaleType",
    "SaleCondition"
  )

  e$iowa.houses.ordinal.vars <- c(
    "LotShape",
    "Utilities",
    "LandSlope",
    "OverallQual",
    "OverallCond",
    "ExterQual",
    "ExterCond",
    "BsmtQual",
    "BsmtCond",
    "BsmtExposure",
    "BsmtFinType1",
    "BsmtFinType2",
    "HeatingQC",
    "Electrical",
    "KitchenQual",
    "Functional",
    "FireplaceQu",
    "GarageFinish",
    "GarageQual",
    "GarageCond",
    "PavedDrive",
    "PoolQC",
    "Fence"
  )

  e$iowa.houses.target.vars <- c(
    "SalePrice"
  )
}