#   Kaggle House
The `KaggleHouse` package is a solution attempt to the Kaggle challenge *House Prices: Advanced Regression Techniques* (for more information click [here](https://https://www.kaggle.com/c/house-prices-advanced-regression-techniques)). It has been developed in the course of the Information Systems Master Ski Seminar in *Applied Machine Learning* (for course information see [here](https://www.wi.uni-muenster.de/student-affairs/course-offerings/228053)).

A lot of the functionality in this package has been tailored to solve the challenge at hand. Nevertheless it has been an aim to develop the package in a sufficiently general way that it should help to tackle other challenges as well (with only few necessary adaptations).

##  Installation
To install the `KaggleHouse` package you need to have the `devtools` package installed. Then you can execute the following command, which will download and install the package:

```r
devtools::install_github(  "https://github.com/MarcoNiemann/kaggle_house","KaggleHouse")
```

##  Quick Run-Through
After installing the package you should ensure that it is actually loaded within your `R` environment:

```r
library(KaggleHouse)
```

Now you should already have access to the `train` and `test` data sets of the challenge which you can view and analyze as follows:

```r
# Show the training data set.
View(data_train)
# Show the test data set.
View(data_test)
```

Since a lot of values are missing and most learners actually require an `NA` free dataset, the `KaggleHouse` package provides functionality to impute the missing values. 

```r
# Load mice library for imputation.
library(mice)
# Fix missing data.
run_generate_data()
```

After the imputation and cleaning run, you should have a number of new data sets available:

| Data Set        | Conducted Changes         | 
| ------------- |-------------| 
| `data_train_none`                  | Substitute missing values with the charater 'None' for a specific set of features(look into the official data documentation for more detail) | 
| `data_train_numeric`               | `data_train_none` where each column is transformed into a numeric vector    | 
| `data_train_numeric_clean`         | Removed four rows which  are stated to be outlier and the column Utilities from the `data_train_numeric_clean` dataset    | 
| `data_train_numeric_clean_imputed` | `data_train_numeric_clean` dataset where all still missing values are either imputed with mice or manually|  

While we provide this multitude of data sets, the most commonly used one for any further progress is `data_train_numeric_clean_imputed`. If you want to create the learner and predictions that were used to participate in the *House Prices: Advanced Regression Techniques* challenge you can execute the following lines:

```r
# Execute the combined learner executing a Boruta Feature
# Selection before computing the xgboost, glm and deep learning
# learners.
# CAUTION: Execution could take multiple hours!
run_learner()
```

In case you want to conduct more experimentation we recommend you to read both our documentation as well as the code documentation accompanying this package. There you will find additional information about the implemented feature selectors and learners, as well as guidance on how to use them.

##  Further Information
For a more detailed insight to the functionality of the `KaggleHouse` package, as well as a complete reasoning for the conducted steps please refer to the appended documentation. 

* Code Documentation: [documentation.pdf](documentation/kaggle_house.pdf)
* Seminar Documentation: [thesis.pdf](documentation/kaggle_house_thesis.pdf)
* Seminar Presentation: [presentation.pdf](documentation/kaggle_house_presentation.pdf)

##  Report an Issue
In case you find an issue in the code please open an issue in the issue tracker. 

## Copyright
Copyright (c) 2016-2017 Frederik Elischberger und Marco Niemann